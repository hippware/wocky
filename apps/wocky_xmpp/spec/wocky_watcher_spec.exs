defmodule :wocky_watcher_spec do
  use ESpec, async: true
  use Wocky.JID

  import CustomAssertions
  import :wocky_watcher

  alias Wocky.Repo.Factory
  alias Wocky.Repo.ID
  alias Wocky.User

  before do
    class = String.to_atom(ID.new)
    host = "localhost"
    result = register(class, host)
    {:ok,
     class: class,
     host: host,
     result: result}
  end

  describe "register/2" do
    it "should return :ok" do
      shared.result |> should(eq :ok)
    end

    it "should create an empty watcher list" do
      shared.class
      |> table()
      |> :mnesia.dirty_all_keys
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
      user = Factory.build(:user)
      jid = User.to_jid(user, ID.new)
      object = JID.make(ID.new, shared.server, ID.new)
      result = watch(shared.class, jid, object)
      {:ok,
        user: user,
        jid: jid,
        object: object,
        result: result
      }
    end

    describe "watch/3" do
      it "should return :ok" do
        shared.result |> should(eq :ok)
      end

      it "should crash with an unregistered class" do
        fn() ->
          watch(String.to_atom(ID.new), shared.jid, shared.object)
        end
        |> should(cause_exit())
      end

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

      it "should crash with an unregistered class" do
        fn() ->
          unwatch(String.to_atom(ID.new), shared.jid, shared.object)
        end
        |> should(cause_exit())
      end

      it "should remove the specified item from the watchers" do
        watchers(shared.class, shared.object)
        |> should(eq [])
      end
    end

    describe "watchers/2" do
      before do
        [j1, j2, j3] =
          3
          |> Factory.build_list(:user)
          |> Enum.map(&User.to_jid(&1, ID.new))

        other_object = JID.make(ID.new, shared.server, ID.new)
        other_class = String.to_atom(ID.new)
        register(other_class, shared.host)

        watch(shared.class, j1, shared.object)
        watch(other_class, j2, shared.object)
        watch(shared.class, j3, other_object)

        result = watchers(shared.class, shared.object)
        {:ok,
         result: result,
         same_group: j1,
         different_class: j2,
         different_object: j3
        }
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
        shared.class
        |> table()
        |> :mnesia.dirty_all_keys
        |> should(eq [])
      end
    end

    describe "unset_presence_hook", async: false do
      before do
        resource = ID.new
        remaining_user =
          :user
          |> Factory.build
          |> User.to_jid(resource)
        watch(shared.class, remaining_user, shared.object)
        :ejabberd_hooks.run(
          :unset_presence_hook, shared.server,
          [shared.user.id, shared.server, jid(shared.jid, :lresource),
           "disconnected"])
        {:ok,
          remaining_user: remaining_user
        }
      end

      it "should clear out only the user for whome the presence was unset" do
        watchers(shared.class, shared.object)
        |> should(eq [shared.remaining_user])
      end
    end
  end
end
