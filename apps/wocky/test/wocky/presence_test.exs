defmodule Wocky.PresenceTest do
  use Wocky.DataCase, async: false

  import Eventually

  alias Wocky.Account.User
  alias Wocky.CallbackManager
  alias Wocky.Presence
  alias Wocky.Presence.Manager
  alias Wocky.Repo.Factory
  alias Wocky.Roster

  setup do
    [user, requestor] = Factory.insert_list(2, :user)
    Roster.befriend(user, requestor)

    {:ok, requestor: requestor, user: user}
  end

  describe "basic presence registration/deregistration" do
    test "presence registration", ctx do
      assert Presence.get(ctx.user, ctx.requestor).status == :offline

      {_, _} = connect(ctx.user)
      assert Presence.get(ctx.user, ctx.requestor).status == :offline

      Presence.set_status(ctx.user, :online)
      assert_eventually(Presence.get(ctx.user, ctx.requestor).status == :online)

      # Set the user's presence to offline and wait for it to register.
      # Doing this prevents a crash due to a db connection ownership error.
      Presence.set_status(ctx.user, :offline)

      assert_eventually(
        Presence.get(ctx.user, ctx.requestor).status == :offline
      )
    end

    test "presence deregistration on connection close", ctx do
      {conn, []} = connect(ctx.user)
      assert Presence.get(ctx.user, ctx.requestor).status == :offline

      Presence.set_status(ctx.user, :online)
      assert_eventually(Presence.get(ctx.user, ctx.requestor).status == :online)
      close_conn(conn)

      assert_eventually(
        Presence.get(ctx.user, ctx.requestor).status == :offline
      )
    end

    test "can get presence on self", ctx do
      {_, _} = connect(ctx.user)
      assert Presence.get(ctx.user, ctx.user).status == :offline

      Presence.set_status(ctx.user, :online)
      assert_eventually(Presence.get(ctx.user, ctx.user).status == :online)

      # Set the user's presence to offline and wait for it to register.
      # Doing this prevents a crash due to a db connection ownership error.
      Presence.set_status(ctx.user, :offline)

      assert_eventually(Presence.get(ctx.user, ctx.user).status == :offline)
    end
  end

  describe "initial manager response" do
    test "initial return value to `connect` should list online friends", ctx do
      uid = ctx.user.id

      assert {_, []} = connect(ctx.user)
      Presence.set_status(ctx.user, :online)

      assert {_, [%User{id: ^uid}]} = connect(ctx.requestor)
    end
  end

  describe "publishing callback" do
    setup ctx do
      self = self()

      original_callbacks = CallbackManager.get(Presence)

      Presence.register_callback(fn u, c_id ->
        send(self, {:presence, u, c_id})
      end)

      uid = ctx.user.id

      {_, []} = connect(ctx.user)
      Presence.set_status(ctx.user, :online)

      {conn_pid, [%User{id: ^uid}]} = connect(ctx.requestor)
      Presence.set_status(ctx.requestor, :online)

      on_exit(fn ->
        CallbackManager.set(Presence, original_callbacks)
      end)

      {:ok, conn_pid: conn_pid}
    end

    test "should publish an online state when a user sets themselves online",
         ctx do
      rid = ctx.requestor.id
      uid = ctx.user.id

      assert_receive {:presence, %User{id: ^rid, presence: %{status: :online}},
                      ^uid}
    end

    test "should not publish again when user sets their state online again",
         ctx do
      rid = ctx.requestor.id
      uid = ctx.user.id

      assert_receive {:presence, %User{id: ^rid, presence: %{status: :online}},
                      ^uid}

      Presence.set_status(ctx.requestor, :online)

      refute_receive {:presence, %User{id: ^rid, presence: %{status: :online}},
                      ^uid}
    end

    test "should publish an offline state when a user sets themselves offline",
         ctx do
      rid = ctx.requestor.id
      uid = ctx.user.id

      assert_receive {:presence, %User{id: ^rid, presence: %{status: :online}},
                      ^uid}

      Presence.set_status(ctx.requestor, :offline)

      assert_receive {:presence, %User{id: ^rid, presence: %{status: :offline}},
                      ^uid}
    end

    test "should not publish again if the user sets themselves offline twice",
         ctx do
      rid = ctx.requestor.id
      uid = ctx.user.id

      assert_receive {:presence, %User{id: ^rid, presence: %{status: :online}},
                      ^uid}

      Presence.set_status(ctx.requestor, :offline)

      assert_receive {:presence, %User{id: ^rid, presence: %{status: :offline}},
                      ^uid}

      Presence.set_status(ctx.requestor, :offline)

      refute_receive {:presence, %User{id: ^rid, presence: %{status: :offline}},
                      ^uid}
    end

    test "should publish an offline state when a user disconnects", ctx do
      rid = ctx.requestor.id
      uid = ctx.user.id

      assert_receive {:presence, %User{id: ^rid, presence: %{status: :online}},
                      ^uid}

      close_conn(ctx.conn_pid)

      assert_receive {:presence, %User{id: ^rid, presence: %{status: :offline}},
                      ^uid}
    end
  end

  describe "race condition tests" do
    test "get_presence race", ctx do
      {:ok, manager} = Manager.acquire(ctx.user)
      send(manager, {:exit_after, 1000})

      assert %Presence{status: :offline} =
               Manager.get_presence(manager, ctx.user)
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
