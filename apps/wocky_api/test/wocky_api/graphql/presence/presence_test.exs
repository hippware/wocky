defmodule WockyAPI.GraphQL.Presence.PresenceTest do
  use WockyAPI.SubscriptionCase, async: false

  import Wocky.Eventually
  import WockyAPI.ChannelHelper

  alias Wocky.Repo.Factory
  alias Wocky.Roster
  alias WockyAPI.Presence

  setup_all do
    Ecto.Adapters.SQL.Sandbox.mode(Wocky.Repo, :auto)

    on_exit(fn ->
      Application.stop(:wocky_db_watcher)
      Wocky.Repo.delete_all(Wocky.User)
    end)
  end

  setup shared do
    [friend, follower, followee, stranger] = Factory.insert_list(4, :user)

    Roster.befriend(shared.user.id, friend.id)
    Roster.follow(follower.id, shared.user.id)
    Roster.follow(shared.user.id, followee.id)

    {:ok,
     friend: friend, follower: follower, followee: followee, stranger: stranger}
  end

  describe "basic presence registration/deregistration" do
    test "presence registration", %{user: user} do
      assert Presence.user_status(user) == :offline
      {_, []} = connect(user)

      assert Presence.user_status(user) == :online
    end

    test "presence deregistration on connection close", %{user: user} do
      {conn, []} = connect(user)
      close_conn(conn)

      assert_eventually(Presence.user_status(user) == :offline)
    end
  end

  describe "initial connection state" do
    setup shared do
      Enum.each(
        [shared.friend, shared.follower, shared.followee, shared.stranger],
        &connect/1
      )

      :ok
    end

    test "set should include online friends and followees", shared do
      {_, online_contacts} = connect(shared.user)

      assert length(online_contacts) == 2
      assert includes_user(online_contacts, shared.friend)
      assert includes_user(online_contacts, shared.followee)
    end
  end

  @subscription """
  subscription {
    followees {
      id
      presence_status
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
      refute_push "subscription:data", _push, 500
    end

    test "should notify when a friend or followee comes online", shared do
      connect(shared.friend)

      assert_push "subscription:data", push, 500
      assert_presence_notification(push.result.data, shared.friend.id, :online)
    end

    test "should only give one notification even if there are multiple connections",
         shared do
      Enum.each(1..5, fn _ -> connect(shared.friend) end)

      assert_push "subscription:data", push, 500
      assert_presence_notification(push.result.data, shared.friend.id, :online)
      refute_push "subscription:data", _push, 500
    end

    test "should not notify when other users come online", shared do
      connect(shared.stranger)

      refute_push "subscription:data", _push, 500
    end

    test "should notify when a friend or followee go offline", shared do
      {conn, _} = connect(shared.followee)

      assert_push "subscription:data", push, 500

      close_conn(conn)

      assert_push "subscription:data", push, 500

      assert_presence_notification(
        push.result.data,
        shared.followee.id,
        :offline
      )
    end

    test "should not notify when other users go offline", shared do
      {conn, _} = connect(shared.follower)

      refute_push "subscription:data", _push, 500

      close_conn(conn)

      refute_push "subscription:data", _push, 500
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

      assert_push "subscription:data", push, 500

      Enum.each(
        0..3,
        fn i ->
          close_conn(Enum.at(conns, i))
        end
      )

      refute_push "subscription:data", _push, 500

      close_conn(Enum.at(conns, 4))
      assert_push "subscription:data", push, 500

      assert_presence_notification(push.result.data, shared.friend.id, :offline)
    end
  end

  describe "live updates to already-connected contacts" do
    setup shared do
      {conn, _} = connect(shared.friend)

      authenticate(shared.user.id, shared.token, shared.socket)
      ref = push_doc(shared.socket, @subscription)
      assert_reply ref, :ok, %{subscriptionId: subscription_id}, 1000

      assert_push "subscription:data", push, 500
      assert_presence_notification(push.result.data, shared.friend.id, :online)

      {:ok, ref: ref, conn: conn}
    end

    test "connected contact disconnects", shared do
      close_conn(shared.conn)

      assert_push "subscription:data", push, 500

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
    assert %{
             "followees" => %{
               "id" => user_id,
               "presence_status" => type |> to_string() |> String.upcase()
             }
           } == data
  end
end
