defmodule Wocky.User.Presence.Manager do
  @moduledoc """
  Per-user manager process for contact presence reporting.
  """

  defmodule State do
    @moduledoc false

    defstruct online_pid: nil,
              mon_refs: [],
              user: nil,
              # BiMap of (User.id -> reference()):
              contact_refs: []
  end

  use GenServer

  alias Phoenix.PubSub
  alias Wocky.{Repo, Roster, User}
  alias Wocky.User.Presence
  alias Wocky.User.Presence.{OnlineProc, Store, Supervisor}

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

  def init(user) do
    _ = PubSub.subscribe(:presence, pubsub_topic(user))
    Store.add_self(user.id)

    {:ok, contact_refs} =
      Repo.transaction(fn ->
        user
        |> Roster.friends_query(user)
        |> Repo.stream()
        |> Enum.reduce(BiMap.new(), &maybe_monitor/2)
      end)

    {:ok, %State{user: user, contact_refs: contact_refs}}
  end

  @spec register_sock(pid()) :: :ok
  def register_sock(manager) do
    GenServer.call(manager, {:register_sock, self()})
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
  def get_presence(manager, user) do
    GenServer.call(manager, {:get_presence, user.id})
  end

  def handle_info({:DOWN, ref, :process, _, _}, s) do
    cond do
      Enum.member?(s.mon_refs, ref) ->
        # A monitored connection process is down - the user has disconnected
        delete_own_connection(ref, s)

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

  def handle_info(_, s) do
    {:noreply, s}
  end

  def handle_call({:register_sock, sock_pid}, _from, %{mon_refs: mon_refs} = s) do
    ref = Process.monitor(sock_pid)
    {:reply, :ok, %{s | mon_refs: [ref | mon_refs]}}
  end

  def handle_call(:online_contacts, _from, %{contact_refs: contact_refs} = s) do
    {:reply, BiMap.keys(contact_refs), s}
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

    Repo.transaction(fn ->
      user
      |> Roster.friends_query(user)
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
    {:reply, :ok, %{s | online_pid: nil}}
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

  defp delete_own_connection(ref, %{mon_refs: mon_refs} = s) do
    case List.delete(mon_refs, ref) do
      [] ->
        Store.remove(s.user.id)
        {:stop, :normal, s}

      new_refs ->
        {:noreply, %{s | mon_refs: new_refs}}
    end
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
    case User.get_user(contact_id) do
      nil ->
        :ok

      contact ->
        Presence.publish(user.id, contact, :offline)
    end
  end
end
