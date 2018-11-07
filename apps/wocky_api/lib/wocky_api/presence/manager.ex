defmodule WockyAPI.Presence.Manager do
  @moduledoc """
  Per-user manager process for contact presence reporting.
  """

  defmodule State do
    @moduledoc false

    defstruct [
      :mon_refs,
      :user_id,
      # BiMap of (User.id -> reference()):
      :contact_refs
    ]
  end

  use GenServer

  alias Phoenix.PubSub
  alias Wocky.{Repo, Roster, User}
  alias WockyAPI.Presence
  alias WockyAPI.Presence.Store

  def acquire(user) do
    Store.transaction(user.id, fn -> get_or_create(user) end)
  end

  defp get_or_create(user) do
    case Store.get(user.id) do
      nil ->
        GenServer.start_link(__MODULE__, {user, self()})

      manager ->
        register(manager)
        {:ok, manager}
    end
  end

  def init({user, sock_pid}) do
    mon_ref = Process.monitor(sock_pid)
    PubSub.subscribe(:presence, pubsub_topic(user))
    Store.add_self(user.id)

    Repo.transaction(fn ->
      user.id
      |> Roster.followers_query(user.id, false)
      |> Repo.stream()
      |> Stream.each(fn u ->
        PubSub.broadcast(:presence, pubsub_topic(u), {:online, user, self()})
      end)
      |> Stream.run()
    end)

    contact_refs =
      user.id
      |> Roster.followees(false)
      |> Enum.reduce(BiMap.new(), &maybe_monitor/2)

    {:ok, %{mon_refs: [mon_ref], user_id: user.id, contact_refs: contact_refs}}
  end

  def register(manager) do
    GenServer.call(manager, {:register, self()})
  end

  @doc "Get the current set of followees we know to be online"
  @spec online_contacts(pid()) :: [User.id()]
  def online_contacts(manager) do
    GenServer.call(manager, :online_contacts)
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

  # One of our followees has come online
  def handle_info({:online, contact, pid}, s) do
    new_refs =
      case BiMap.get(s.contact_refs, contact.id) do
        nil ->
          # We had them marked as offline - set up tracking for their presence
          ref = Process.monitor(pid)
          Presence.publish(s.user_id, contact)
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

  def handle_call(:online_contacts, _from, %{contact_refs: contact_refs} = s) do
    {:reply, BiMap.keys(contact_refs), s}
  end

  def handle_call({:register, pid}, _from, s) do
    ref = Process.monitor(pid)
    {:reply, :ok, %{s | mon_refs: [ref | s.mon_refs]}}
  end

  defp maybe_monitor(target, acc) do
    case Store.get(target.id) do
      nil ->
        acc

      pid ->
        ref = Process.monitor(pid)
        BiMap.put(acc, target.id, ref)
    end
  end

  defp pubsub_topic(user), do: "user:" <> user.id

  defp delete_own_connection(ref, %{mon_refs: mon_refs} = s) do
    case List.delete(mon_refs, ref) do
      [] ->
        Store.remove(s.user_id)
        {:stop, :normal, s}

      new_refs ->
        {:noreply, %{s | mon_refs: new_refs}}
    end
  end

  defp delete_contact_presence(ref, %{contact_refs: contact_refs} = s) do
    # Clean up the tracking record
    contact_refs
    |> BiMap.get_key(ref)
    |> send_offline(s.user_id)

    new_refs = BiMap.delete_value(contact_refs, ref)

    {:noreply, %{s | contact_refs: new_refs}}
  end

  defp send_offline(contact_id, user_id) do
    case User.get_user(contact_id) do
      nil ->
        :ok

      contact ->
        Presence.publish(user_id, contact)
    end
  end
end
