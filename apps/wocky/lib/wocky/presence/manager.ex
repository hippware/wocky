defmodule Wocky.Presence.Manager do
  @moduledoc """
  Per-user manager process for contact presence reporting.
  """

  @max_register_retries 5

  defmodule State do
    @moduledoc false

    alias Wocky.Account.User

    defstruct [
      :user,
      :contact_refs,
      online_pid: nil,
      handler_mon_refs: %{},
      socket_mon_refs: %{}
    ]

    @type t :: %__MODULE__{
            # The user to which this presence manager belongs
            user: User.t(),
            # BiMap of (User.id() -> reference()) containing a user's
            # friends and their online processes, if present.
            contact_refs: BiMap.t(),
            # Pid (if any) representing this user's online state
            online_pid: nil | pid(),
            # Map of (reference() -> pid()) containing monitor references
            # and their respective pids for a user's open graphql handlers (if any)
            handler_mon_refs: %{optional(reference()) => pid()},
            # Map of (reference() -> pid()) containing monitor references
            # and their respective pids for a user's open sockets (if any)
            socket_mon_refs: %{optional(reference()) => pid()}
          }
  end

  use GenServer, restart: :temporary

  alias Phoenix.PubSub
  alias Wocky.Account
  alias Wocky.Account.User
  alias Wocky.Friends
  alias Wocky.Presence
  alias Wocky.Presence.ConnectionEvent
  alias Wocky.Presence.OnlineProc
  alias Wocky.Presence.Store
  alias Wocky.Presence.Supervisor
  alias Wocky.Repo

  def acquire(user) do
    Store.transaction(user.id, fn -> get_or_create(user) end)
  end

  defp get_or_create(user) do
    case Store.get_manager(user.id) do
      nil ->
        Supervisor.start_child(user)

      manager when is_pid(manager) ->
        {:ok, manager}
    end
  end

  def start_link(user), do: GenServer.start_link(__MODULE__, user)

  @spec stop(User.t()) :: :ok
  def stop(user) do
    case Store.get_manager(user.id) do
      nil ->
        :ok

      manager when is_pid(manager) ->
        safe_op(fn -> GenServer.stop(manager, :normal) end, fn -> :ok end)
    end
  end

  @spec stop_all() :: :ok
  def stop_all do
    Supervisor
    |> DynamicSupervisor.which_children()
    |> Enum.map(&elem(&1, 1))
    |> Enum.filter(&is_pid/1)
    |> Enum.each(&DynamicSupervisor.terminate_child(Supervisor, &1))
  end

  @impl true
  def init(user) do
    _ = PubSub.subscribe(:presence, pubsub_topic(user))
    Store.add_self(user.id)

    {:ok, contact_refs} =
      Repo.transaction(fn ->
        user
        |> Friends.friends_query(user)
        |> Repo.stream()
        |> Enum.reduce(BiMap.new(), &maybe_monitor/2)
      end)

    {:ok, %State{user: user, contact_refs: contact_refs}}
  end

  @spec register_handler(User.t()) :: pid()
  def register_handler(user), do: do_register_handler(user, 0)

  @spec register_socket(pid(), pid()) :: :ok
  def register_socket(manager, socket_pid) do
    GenServer.call(manager, {:register_socket, socket_pid})
  end

  # Work around a race condition here: if the manager is acquired, then dies
  # before handling the :register_handler, we'll get an :exit thrown. In this
  # case retrying should create us a new, non-dead manager which should work
  # fine.
  defp do_register_handler(_user, @max_register_retries),
    do: exit(:presence_manger_not_acquired)

  defp do_register_handler(user, retries) do
    {:ok, manager} = acquire(user)

    safe_op(
      fn ->
        GenServer.call(manager, {:register_handler, self()})
        manager
      end,
      fn -> do_register_handler(user, retries + 1) end
    )
  end

  @spec get_sockets(pid()) :: [pid()]
  def get_sockets(manager) do
    # This is often called as a user is disconnecting
    safe_call(manager, :get_sockets, [])
  end

  @spec set_status(pid(), Presence.status()) :: :ok
  def set_status(manager, status) do
    GenServer.call(manager, {:set_status, status})
  end

  @doc "Get the current set of followees we know to be online"
  @spec online_contacts(pid()) :: [User.id()]
  def online_contacts(manager) do
    GenServer.call(manager, :online_contacts)
  end

  @doc "Get the online/offline status of a contact"
  @spec get_presence(pid(), User.t()) :: Presence.t()
  def get_presence(manager, user),
    do:
      safe_call(
        manager,
        {:get_presence, user.id},
        Presence.make_presence(:offline)
      )

  @impl true
  def handle_info({:DOWN, ref, :process, _, _}, s) do
    cond do
      Map.has_key?(s.handler_mon_refs, ref) ->
        # A monitored connection process is down - the user has disconnected.
        ref
        |> delete_own_handler(s)
        |> maybe_shutdown()

      Map.has_key?(s.socket_mon_refs, ref) ->
        # A registered socket has closed. Remove it from our records.
        signal_connection(s.user, :disconnected)

        %{s | socket_mon_refs: Map.delete(s.socket_mon_refs, ref)}
        |> maybe_shutdown()

      BiMap.has_value?(s.contact_refs, ref) ->
        # A tracked presence process for a contact has gone down.
        delete_contact_presence(ref, s)

      true ->
        {:noreply, s}
    end
  end

  # One of our friends has come online
  def handle_info({:online, contact, pid}, s) do
    new_refs =
      case BiMap.get(s.contact_refs, contact.id) do
        nil ->
          # We had them marked as offline - set up tracking for their presence
          ref = Process.monitor(pid)
          Presence.publish(s.user.id, contact, :online)
          BiMap.put(s.contact_refs, contact.id, ref)

        _ref ->
          # We already had them marked as online - do nothing
          s.contact_refs
      end

    {:noreply, %{s | contact_refs: new_refs}}
  end

  # Used for testing exit-related race - blocks the process while keeping it
  # alive, then exits
  def handle_info({:exit_after, time}, s) do
    Process.sleep(time)
    {:stop, :normal, s}
  end

  def handle_info(_, s) do
    {:noreply, s}
  end

  @impl true
  def handle_call(
        {:register_handler, handler_pid},
        _from,
        %{handler_mon_refs: handler_mon_refs} = s
      ) do
    ref = Process.monitor(handler_pid)

    {:reply, :ok,
     %{s | handler_mon_refs: Map.put(handler_mon_refs, ref, handler_pid)}}
  end

  def handle_call(
        {:register_socket, socket_pid},
        _from,
        %{socket_mon_refs: socket_mon_refs} = s
      ) do
    if socket_mon_refs == %{}, do: signal_connection(s.user, :connected)

    ref = Process.monitor(socket_pid)

    {:reply, :ok,
     %{s | socket_mon_refs: Map.put(socket_mon_refs, ref, socket_pid)}}
  end

  def handle_call(:online_contacts, _from, %{contact_refs: contact_refs} = s) do
    {:reply, BiMap.keys(contact_refs), s}
  end

  def handle_call(
        {:get_presence, user_id},
        _from,
        %{online_pid: online_pid, user: %User{id: user_id}} = s
      ) do
    status = if online_pid, do: :online, else: :offline
    {:reply, Presence.make_presence(status), s}
  end

  def handle_call(
        {:get_presence, contact_id},
        _from,
        %{contact_refs: contact_refs} = s
      ) do
    status =
      case BiMap.get(contact_refs, contact_id) do
        nil -> :offline
        _ -> :online
      end

    {:reply, Presence.make_presence(status), s}
  end

  def handle_call(
        {:set_status, :online},
        _from,
        %{online_pid: nil, user: user} = s
      ) do
    {:ok, online_pid} = OnlineProc.start_link()

    Store.set_self_online(user.id, online_pid)
    Presence.publish(user.id, user, :online)

    Repo.transaction(fn ->
      user
      |> Friends.friends_query(user)
      |> Repo.stream()
      |> Stream.each(fn u ->
        PubSub.broadcast(
          :presence,
          pubsub_topic(u),
          {:online, user, online_pid}
        )
      end)
      |> Stream.run()
    end)

    {:reply, :ok, %{s | online_pid: online_pid}}
  end

  def handle_call({:set_status, :online}, _from, s) do
    # Already online
    {:reply, :ok, s}
  end

  def handle_call({:set_status, :offline}, _from, %{online_pid: nil} = s) do
    # Already offline
    {:reply, :ok, s}
  end

  def handle_call(
        {:set_status, :offline},
        _from,
        %{online_pid: online_pid, user: user} = s
      ) do
    OnlineProc.go_offline(online_pid)
    Store.add_self(user.id)
    Presence.publish(user.id, user, :offline)
    {:reply, :ok, %{s | online_pid: nil}}
  end

  def handle_call(:get_sockets, _from, s) do
    {:reply, Map.values(s.socket_mon_refs), s}
  end

  defp maybe_monitor(target, acc) do
    case Store.get_online(target.id) do
      nil ->
        acc

      pid when is_pid(pid) ->
        ref = Process.monitor(pid)
        BiMap.put(acc, target.id, ref)
    end
  end

  defp pubsub_topic(user), do: "user:" <> user.id

  defp delete_own_handler(ref, %{handler_mon_refs: handler_mon_refs} = s) do
    case Map.delete(handler_mon_refs, ref) do
      m when m == %{} ->
        Store.remove(s.user.id)
        # No need to publish to ourselves here - by definition we can't have any
        # live connections to receive the message on
        %{s | handler_mon_refs: m}

      new_refs ->
        %{s | handler_mon_refs: new_refs}
    end
  end

  defp signal_connection(user, status) do
    _ =
      %ConnectionEvent{user: user, status: status}
      |> Dawdle.signal(direct: true)

    :ok
  end

  defp delete_contact_presence(ref, %{contact_refs: contact_refs} = s) do
    # Clean up the tracking record
    contact_refs
    |> BiMap.get_key(ref)
    |> send_offline(s.user)

    new_refs = BiMap.delete_value(contact_refs, ref)

    {:noreply, %{s | contact_refs: new_refs}}
  end

  defp send_offline(contact_id, user) do
    case Account.get_user(contact_id) do
      nil ->
        :ok

      contact ->
        Presence.publish(user.id, contact, :offline)
    end
  end

  # Call an existing manager - if the manager exits during the call, return
  # the supplied default value.
  defp safe_call(manager, call, default),
    do: safe_op(fn -> GenServer.call(manager, call) end, fn -> default end)

  defp safe_op(op, fallback) do
    op.()
  catch
    :exit, _ -> fallback.()
  end

  defp maybe_shutdown(
         %{handler_mon_refs: handlers, socket_mon_refs: sockets} = s
       ) do
    if handlers == %{} and sockets == %{} do
      {:stop, :normal, s}
    else
      {:noreply, s}
    end
  end
end
