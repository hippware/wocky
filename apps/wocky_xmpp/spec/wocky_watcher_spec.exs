defmodule :wocky_watcher_spec do
  use ESpec, async: true
  use Wocky.JID

  import :wocky_watcher

  alias Wocky.Repo.Factory
  alias Wocky.Repo.ID
  alias Wocky.User

  before do
    class = String.to_atom(ID.new())
    host = "localhost"
    result = register(class, host)
    {:ok, class: class, host: host, result: result}
  end

  describe "register/2" do
    it "should return :ok" do
      shared.result |> should(eq :ok)
    end

    it "should create an empty watcher list" do
      :ejabberd_redis.cmd(["KEYS", key(shared.class)])
      |> should(eq [])
    end
  end

  describe "unregister/2" do
    it "should return :ok" do
      unregister(shared.class, shared.host) |> should(eq :ok)
    end
  end

  describe "watch/unwatch/watchers" do
    before do
      jid = make_user_jid()
      object = JID.make(ID.new(), shared.server, ID.new())
      result = watch(shared.class, jid, object)
      {:ok, jid: jid, object: object, result: result}
    end

    describe "watch/3" do
      it "should return :ok" do
        shared.result |> should(eq :ok)
      end

      # This no longer happens with Redis. TODO: Revisit.
      #      it "should crash with an unregistered class" do
      #  fn() ->
      #    watch(String.to_atom(ID.new), shared.jid, shared.object)
      #  end
      #  |> should(cause_exit())
      # end

      it "should add an item to the table" do
        watchers(shared.class, shared.object)
        |> should(eq [shared.jid])
      end
    end

    describe "unwatch/3" do
      before do
        result = unwatch(shared.class, shared.jid, shared.object)
        {:ok, result: result}
      end

      it "should return :ok" do
        shared.result |> should(eq :ok)
      end

      # This no longer happens with Redis. TODO: Revisit.
      # it "should crash with an unregistered class" do
      #  fn() ->
      #    unwatch(String.to_atom(ID.new), shared.jid, shared.object)
      #  end
      #  |> should(cause_exit())
      # end

      it "should remove the specified item from the watchers" do
        watchers(shared.class, shared.object)
        |> should(eq [])
      end
    end

    describe "unwatch_all/2" do
      before do
        other_jid = make_user_jid()
        watch(shared.class, other_jid, shared.object)

        objects =
          Enum.map(1..2, fn _ ->
            object = JID.make(ID.new(), shared.server, ID.new())
            watch(shared.class, shared.jid, object)
            object
          end)

        unwatch_all(shared.class, shared.jid)
        {:ok, other_jid: other_jid, objects: [shared.object | objects]}
      end

      it "should clear out all watches for a user" do
        refute(
          Enum.any?(shared.objects, fn o ->
            watchers(shared.class, o)
            |> Enum.member?(shared.jid)
          end)
        )
      end

      it "should not remove other watchers" do
        watchers(shared.class, shared.object) |> should(have shared.other_jid)
      end
    end

    describe "watchers/2" do
      before do
        [j1, j2, j3] = Enum.map(1..3, fn _ -> make_user_jid() end)

        other_object = JID.make(ID.new(), shared.server, ID.new())
        other_class = String.to_atom(ID.new())
        register(other_class, shared.host)

        watch(shared.class, j1, shared.object)
        watch(other_class, j2, shared.object)
        watch(shared.class, j3, other_object)

        result = watchers(shared.class, shared.object)

        {:ok,
         result: result,
         same_group: j1,
         different_class: j2,
         different_object: j3}
      end

      it "should return the users in the group" do
        shared.result |> should(have shared.jid)
        shared.result |> should(have shared.same_group)
      end

      it "should not have users from other classes" do
        shared.result |> should_not(have shared.different_class)
      end

      it "should not have users from different objects" do
        shared.result |> should_not(have shared.different_object)
      end
    end

    describe "node_cleanup hook", async: false do
      before do
        :ejabberd_hooks.run(:node_cleanup, :global, [node()])
      end

      it "should clear out all memebers of the table on this node" do
        :ejabberd_redis.cmd(["KEYS", key(shared.class)])
        |> should(eq [])
      end
    end

    describe "unset_presence_hook", async: false do
      before do
        remaining_user = make_user_jid()
        watch(shared.class, remaining_user, shared.object)

        :ejabberd_hooks.run(:unset_presence_hook, shared.server, [
          jid(shared.jid, :luser),
          shared.server,
          jid(shared.jid, :lresource),
          "disconnected"
        ])

        {:ok, remaining_user: remaining_user}
      end

      it "should clear out only the user for whome the presence was unset" do
        watchers(shared.class, shared.object)
        |> should(eq [shared.remaining_user])
      end
    end
  end

  defp make_user_jid() do
    :user
    |> Factory.build()
    |> User.to_jid(ID.new())
  end
end
