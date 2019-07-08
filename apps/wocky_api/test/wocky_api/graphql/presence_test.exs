defmodule WockyAPI.GraphQL.PresenceTest do
  use WockyAPI.SubscriptionCase, async: false

  import Eventually

  alias Wocky.Presence
  alias Wocky.Repo.Factory
  alias Wocky.Roster

  setup_all do
    # Share database access with the Presence Manager process.
    Ecto.Adapters.SQL.Sandbox.mode(Wocky.Repo, :auto)

    on_exit(fn ->
      Wocky.Repo.delete_all(Wocky.Account.User)
    end)
  end

  setup ctx do
    [friend, stranger] = Factory.insert_list(2, :user)

    Roster.befriend(ctx.user, friend)

    {:ok, friend: friend, stranger: stranger}
  end

  describe "initial connection state" do
    setup ctx do
      Enum.each([ctx.friend, ctx.stranger], fn u ->
        connect(u)
        Presence.set_status(u, :online)
      end)
    end

    test "set should include online friends", ctx do
      {_, online_contacts} = connect(ctx.user)

      assert length(online_contacts) == 1
      assert includes_user(online_contacts, ctx.friend)
    end
  end

  @set_status """
  mutation ($status: PresenceStatus!) {
    presenceStatus (input: {status: $status}) {
      successful
    }
  }
  """
  describe "set status" do
    setup ctx do
      authenticate(ctx.user.id, ctx.token, ctx.socket)

      :ok
    end

    test "default state should be offline", ctx do
      assert %Presence{status: :offline} = Presence.get(ctx.user, ctx.friend)
    end

    test "setting online then offline should correctly change status", ctx do
      ref! =
        push_doc(ctx.socket, @set_status, variables: %{"status" => "ONLINE"})

      assert_reply ref!, :ok, _, 150
      assert_eventually(Presence.get(ctx.user, ctx.friend).status == :online)

      ref! =
        push_doc(ctx.socket, @set_status, variables: %{"status" => "OFFLINE"})

      assert_reply ref!, :ok, _, 150
      assert_eventually(Presence.get(ctx.user, ctx.friend).status == :offline)
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
    setup ctx do
      authenticate(ctx.user.id, ctx.token, ctx.socket)
      ref = push_doc(ctx.socket, @subscription)
      assert_reply ref, :ok, %{subscriptionId: _subscription_id}, 150

      {:ok, ref: ref}
    end

    test "should give no initial notifications when nobody is online" do
      refute_subscription_update _data, 10
    end

    test "should notify when the user themselves goes online and offline",
         ctx do
      Presence.set_status(ctx.user, :online)

      assert_presence_notification(ctx.user.id, :online)

      Presence.set_status(ctx.user, :offline)

      assert_presence_notification(ctx.user.id, :offline)
    end

    test "should notify when a friend comes online", ctx do
      connect(ctx.friend)
      Presence.set_status(ctx.friend, :online)

      assert_presence_notification(ctx.friend.id, :online)
    end

    test """
         should only give one notification even if there are multiple connections
         and online notifications
         """,
         ctx do
      Enum.each(1..5, fn _ ->
        connect(ctx.friend)
        Presence.set_status(ctx.friend, :online)
      end)

      assert_presence_notification(ctx.friend.id, :online)
      refute_subscription_update _data, 10
    end

    test "should not notify when other users come online", ctx do
      connect(ctx.stranger)
      Presence.set_status(ctx.stranger, :online)

      refute_subscription_update _data, 10
    end

    test "should notify when a friend goes offline via disconnect", ctx do
      {conn, _} = connect(ctx.friend)
      Presence.set_status(ctx.friend, :online)

      assert_subscription_update _data

      close_conn(conn)

      assert_presence_notification(ctx.friend.id, :offline)
    end

    test "should notify when a friend goes offline via status setting", ctx do
      connect(ctx.friend)
      Presence.set_status(ctx.friend, :online)

      assert_subscription_update _data

      Presence.set_status(ctx.friend, :offline)

      assert_presence_notification(ctx.friend.id, :offline)
    end

    test "should not notify when other users go offline via disconnect", ctx do
      {conn, _} = connect(ctx.stranger)
      Presence.set_status(ctx.stranger, :online)

      refute_subscription_update _data, 10

      close_conn(conn)

      refute_subscription_update _data, 10
    end

    test "should not notify when other users go offline via status setting",
         ctx do
      connect(ctx.stranger)
      Presence.set_status(ctx.stranger, :online)

      refute_subscription_update _data, 10

      Presence.set_status(ctx.stranger, :offline)

      refute_subscription_update _data, 10
    end

    test """
         should only send an offline notification when all connections are closed
         """,
         ctx do
      conns =
        Enum.map(
          0..4,
          fn _ ->
            {conn, _} = connect(ctx.friend)
            Presence.set_status(ctx.friend, :online)
            conn
          end
        )

      assert_subscription_update _data

      Enum.each(
        0..3,
        fn i ->
          close_conn(Enum.at(conns, i))
        end
      )

      refute_subscription_update _data, 10

      close_conn(Enum.at(conns, 4))

      assert_presence_notification(ctx.friend.id, :offline)
    end
  end

  describe "live updates to already-connected contacts" do
    setup ctx do
      {conn, _} = connect(ctx.friend)
      Presence.set_status(ctx.friend, :online)

      authenticate(ctx.user.id, ctx.token, ctx.socket)
      ref = push_doc(ctx.socket, @subscription)
      assert_reply ref, :ok, %{subscriptionId: _subscription_id}, 150

      assert_presence_notification(ctx.friend.id, :online)

      {:ok, ref: ref, conn: conn}
    end

    test "connected contact disconnects", ctx do
      close_conn(ctx.conn)

      assert_presence_notification(ctx.friend.id, :offline)
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

  defp assert_presence_notification(user_id, type) do
    expected_status = type |> to_string() |> String.upcase()

    assert_subscription_update %{
      result: %{
        data: %{
          "presence" => %{
            "id" => ^user_id,
            "presence_status" => ^expected_status,
            "presence" => %{
              "status" => ^expected_status,
              "updated_at" => _
            }
          }
        }
      }
    }
  end
end
