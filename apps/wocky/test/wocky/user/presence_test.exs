defmodule Wocky.User.PresenceTest do
  use Wocky.DataCase, async: false

  import Eventually

  alias Wocky.Repo.Factory
  alias Wocky.Roster
  alias Wocky.User
  alias Wocky.User.Presence

  setup do
    [user, requestor] = Factory.insert_list(2, :user)
    Roster.befriend(user, requestor)

    {:ok, requestor: requestor, user: user}
  end

  describe "basic presence registration/deregistration" do
    test "presence registration", ctx do
      assert Presence.get(ctx.user, ctx.requestor).status == :offline
      {_, _} = connect(ctx.user)

      assert Presence.get(ctx.user, ctx.requestor).status == :online
    end

    test "presence deregistration on connection close", ctx do
      {conn, []} = connect(ctx.user)
      close_conn(conn)

      assert_eventually(
        Presence.get(ctx.user, ctx.requestor).status == :offline
      )
    end
  end

  describe "test publishing callback" do
    setup do
      self = self()

      Presence.register_callback(fn u, c_id ->
        send(self, {:presence, u, c_id})
      end)

      on_exit(fn ->
        Presence.clear_callbacks()
      end)
    end

    test "should publish an online state when a user connects", ctx do
      rid = ctx.requestor.id
      uid = ctx.user.id

      assert {_, []} = connect(ctx.user)
      assert {_, [%User{id: ^uid}]} = connect(ctx.requestor)

      assert_receive {:presence, %User{id: ^rid, presence: %{status: :online}},
                      ^uid}
    end

    test "should publish an offline state when a user disconnects", ctx do
      rid = ctx.requestor.id
      uid = ctx.user.id

      assert {_, []} = connect(ctx.user)
      assert {conn_pid, [%User{id: ^uid}]} = connect(ctx.requestor)

      assert_receive {:presence, %User{id: ^rid, presence: %{status: :online}},
                      ^uid}

      close_conn(conn_pid)

      assert_receive {:presence, %User{id: ^rid, presence: %{status: :offline}},
                      ^uid}
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
end
