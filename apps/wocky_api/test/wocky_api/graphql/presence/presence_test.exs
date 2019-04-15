defmodule WockyAPI.GraphQL.Presence.PresenceTest do
  use WockyAPI.SubscriptionCase, async: false

  import WockyAPI.ChannelHelper

  alias Wocky.Repo.Factory
  alias Wocky.Roster
  alias Wocky.User.Presence

  setup_all do
    Ecto.Adapters.SQL.Sandbox.mode(Wocky.Repo, :auto)

    on_exit(fn ->
      Application.stop(:wocky_db_watcher)
      Wocky.Repo.delete_all(Wocky.User)
    end)
  end

  setup shared do
    [friend, stranger] = Factory.insert_list(2, :user)

    Roster.befriend(shared.user, friend)

    {:ok, friend: friend, stranger: stranger}
  end

  describe "initial connection state" do
    setup shared do
      Enum.each([shared.friend, shared.stranger], &connect/1)
    end

    test "set should include online friends", shared do
      {_, online_contacts} = connect(shared.user)

      assert length(online_contacts) == 1
      assert includes_user(online_contacts, shared.friend)
    end
  end

  @subscription """
  subscription {
    presence {
      id
      presence_status
      presence {
        status
        updated_at
      }
    }
  }
  """
  describe "live updates after connection" do
    setup shared do
      authenticate(shared.user.id, shared.token, shared.socket)
      ref = push_doc(shared.socket, @subscription)
      assert_reply ref, :ok, %{subscriptionId: subscription_id}, 1000

      {:ok, ref: ref}
    end

    test "should give no initial notifications when nobody is online" do
      refute_push "subscription:data", _push, 2000
    end

    test "should notify when a friend comes online", shared do
      connect(shared.friend)

      assert_push "subscription:data", push, 2000
      assert_presence_notification(push.result.data, shared.friend.id, :online)
    end

    test "should only give one notification even if there are multiple connections",
         shared do
      Enum.each(1..5, fn _ -> connect(shared.friend) end)

      assert_push "subscription:data", push, 2000
      assert_presence_notification(push.result.data, shared.friend.id, :online)
      refute_push "subscription:data", _push, 2000
    end

    test "should not notify when other users come online", shared do
      connect(shared.stranger)

      refute_push "subscription:data", _push, 2000
    end

    test "should notify when a friend goes offline", shared do
      {conn, _} = connect(shared.friend)

      assert_push "subscription:data", push, 2000

      close_conn(conn)

      assert_push "subscription:data", push, 2000

      assert_presence_notification(
        push.result.data,
        shared.friend.id,
        :offline
      )
    end

    test "should not notify when other users go offline", shared do
      {conn, _} = connect(shared.stranger)

      refute_push "subscription:data", _push, 2000

      close_conn(conn)

      refute_push "subscription:data", _push, 2000
    end

    test """
         should only send an offline notification when all connections are closed
         """,
         shared do
      conns =
        Enum.map(
          0..4,
          fn _ ->
            {conn, _} = connect(shared.friend)
            conn
          end
        )

      assert_push "subscription:data", push, 2000

      Enum.each(
        0..3,
        fn i ->
          close_conn(Enum.at(conns, i))
        end
      )

      refute_push "subscription:data", _push, 2000

      close_conn(Enum.at(conns, 4))
      assert_push "subscription:data", push, 2000

      assert_presence_notification(push.result.data, shared.friend.id, :offline)
    end
  end

  describe "live updates to already-connected contacts" do
    setup shared do
      {conn, _} = connect(shared.friend)

      authenticate(shared.user.id, shared.token, shared.socket)
      ref = push_doc(shared.socket, @subscription)
      assert_reply ref, :ok, %{subscriptionId: subscription_id}, 1000

      assert_push "subscription:data", push, 2000
      assert_presence_notification(push.result.data, shared.friend.id, :online)

      {:ok, ref: ref, conn: conn}
    end

    test "connected contact disconnects", shared do
      close_conn(shared.conn)

      assert_push "subscription:data", push, 2000

      assert_presence_notification(push.result.data, shared.friend.id, :offline)
    end
  end

  defp connect(user) do
    self = self()

    conn_pid =
      spawn_link(fn ->
        users = Presence.connect(user)
        send(self, {:connected, users})

        receive do
          :exit -> :ok
        end
      end)

    receive do
      {:connected, users} -> {conn_pid, users}
    end
  end

  defp close_conn(pid), do: send(pid, :exit)

  defp includes_user(list, user),
    do: Enum.any?(list, &(&1.id == user.id))

  defp assert_presence_notification(data, user_id, type) do
    expected_status = type |> to_string() |> String.upcase()

    assert %{
             "presence" => %{
               "id" => ^user_id,
               "presence_status" => ^expected_status,
               "presence" => %{
                 "status" => ^expected_status,
                 "updated_at" => _
               }
             }
           } = data
  end
end
