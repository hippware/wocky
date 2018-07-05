# credo:disable-for-this-file Credo.Check.Refactor.PipeChainStart
defmodule Wocky.BotSpec do
  use ESpec, async: true
  use ModelHelpers
  use Wocky.JID
  use Wocky.RSMHelper

  alias Faker.Lorem
  alias Wocky.Bot
  alias Wocky.GeoUtils
  alias Wocky.HomeStream
  alias Wocky.HomeStream.Item, as: HomeStreamItem
  alias Wocky.Index.TestIndexer
  alias Wocky.Repo
  alias Wocky.Repo.{Factory, ID, Timestamp}
  alias Wocky.Roster
  alias Wocky.User

  describe "helper functions" do
    let :bot, do: Factory.build(:bot)

    describe "make_node/1" do
      subject do: Bot.make_node(bot())

      it do: should(eq "bot/" <> bot().id)
    end

    describe "to_jid/1" do
      subject do: Bot.to_jid(bot())

      it do: jid(subject(), :luser) |> should(eq "")
      it do: jid(subject(), :lserver) |> should(eq Wocky.host())
      it do: jid(subject(), :lresource) |> should(eq Bot.make_node(bot()))
    end

    describe "get_id_from_jid/1" do
      it do:
           bot() |> Bot.to_jid() |> Bot.get_id_from_jid() |> should(eq bot().id)

      it do: "bogus" |> Bot.get_id_from_jid() |> should(be_nil())

      it do
        "bogus"
        |> JID.make("testing")
        |> Bot.get_id_from_jid()
        |> should(be_nil())
      end
    end

    describe "get_id_from_node/1" do
      it do
        bot()
        |> Bot.make_node()
        |> Bot.get_id_from_node()
        |> should(eq bot().id)
      end

      it do: "bogus" |> Bot.get_id_from_node() |> should(be_nil())
    end

    describe "public?" do
      it do: bot() |> Bot.public?() |> should(eq bot().public)
    end
  end

  describe "validations" do
    let :attrs do
      %{
        id: ID.new(),
        user_id: ID.new(),
        title: "test bot",
        location: GeoUtils.point(5.0, 5.0)
      }
    end

    it "should pass with valid attributes" do
      %Bot{}
      |> Bot.changeset(attrs())
      |> should(be_valid())
    end

    it "should fail with missing fields" do
      %Bot{}
      |> Bot.changeset(%{})
      |> should(have_errors(Map.keys(attrs())))
    end

    it "should fail with negative radius" do
      %Bot{}
      |> Bot.changeset(Map.put(attrs(), :radius, -1))
      |> should(have_errors([:radius]))
    end

    it "should set pending to 'false'" do
      changeset = Bot.changeset(%Bot{}, attrs())
      changeset.changes.pending |> should(be_false())
    end
  end

  describe "database interactions", async: false do
    let :user, do: Factory.insert(:user)
    let! :bot, do: Factory.insert(:bot, user: user())

    before do
      TestIndexer.reset()
    end

    describe "get/2" do
      let! :pending, do: Factory.insert(:bot, user: user(), pending: true)

      it "should return the requested bot" do
        bot().id
        |> Bot.get()
        |> Repo.preload(:user)
        |> should(eq bot())
      end

      it "should work for retrieving by jid" do
        bot()
        |> Bot.to_jid()
        |> Bot.get()
        |> Repo.preload(:user)
        |> should(eq bot())
      end

      it "should return nil for invalid bot jids" do
        ""
        |> JID.make("/notbot/" <> bot().id)
        |> Bot.get()
        |> should(be_nil())
      end

      it "should return nil for non-existant bots" do
        ID.new()
        |> Bot.get()
        |> Repo.preload(:user)
        |> should(be_nil())
      end

      it "should not return pending bots by default" do
        pending().id
        |> Bot.get()
        |> Repo.preload(:user)
        |> should(be_nil())
      end

      it "should return pending bots if specified" do
        pending().id
        |> Bot.get(true)
        |> Repo.preload(:user)
        |> should(eq pending())
      end
    end

    describe "preallocate/2" do
      let :preallocated, do: Bot.preallocate(user().id)

      it "returns a pending bot" do
        preallocated().pending |> should(be_true())
      end

      it "creates a bot in the database" do
        db_bot = Repo.get(Bot, preallocated().id)

        db_bot.user_id |> should(eq preallocated().user_id)
      end

      it "raises on error" do
        fn -> Bot.preallocate(ID.new(), "localhost") end
        |> should(raise_exception())
      end
    end

    describe "insert/1" do
      it "returns an ok result on success" do
        :bot
        |> Factory.params_for(user: user())
        |> Bot.insert()
        |> should(be_ok_result())
      end

      it "returns an error result on failure" do
        %{} |> Bot.insert() |> should(be_error_result())
      end

      context "full text search index" do
        before do
          :bot |> Factory.params_for(user: user()) |> Bot.insert()
          :ok
        end

        it "should be updated" do
          TestIndexer.get_index_operations() |> should_not(be_empty())
        end
      end
    end

    describe "update/2" do
      it "returns an ok result on success" do
        bot()
        |> Bot.update(%{title: "updated bot"})
        |> should(be_ok_result())
      end

      it "returns an error result on failure" do
        %Bot{} |> Bot.update(%{}) |> should(be_error_result())
      end

      context "full text search index" do
        before do
          Bot.update(bot(), %{title: "updated bot"})
          :ok
        end

        it "should be updated" do
          TestIndexer.get_index_operations() |> should_not(be_empty())
        end
      end

      context "out of range location" do
        it "should normalize latitude and longitude" do
          {:ok, %Bot{id: id}} =
            Bot.update(bot(), %{location: GeoUtils.point(-95.0, -185)})

          Repo.get(Bot, id).location |> should(eq GeoUtils.point(-85, 175))
        end
      end

      context "home stream cleanup" do
        before do
          bot = Factory.insert(:bot, %{public: true, user: user()})
          shared_user = Factory.insert(:user)
          unshared_user = Factory.insert(:user)

          Enum.each(
            [user(), shared_user, unshared_user],
            &Factory.insert(:home_stream_item, %{reference_bot: bot, user: &1})
          )

          Factory.insert(:share, %{sharer: user(), user: shared_user, bot: bot})

          {:ok,
           bot: bot, shared_user: shared_user, unshared_user: unshared_user}
        end

        context "bot becomes private" do
          before do
            Bot.update(shared.bot, %{public: false})
            :ok
          end

          it do:
               shared.shared_user.id
               |> HomeStream.get()
               |> is_deleted()
               |> should(be_false())

          it do:
               shared.unshared_user.id
               |> HomeStream.get()
               |> is_deleted()
               |> should(be_true())

          it do:
               user().id
               |> HomeStream.get()
               |> is_deleted()
               |> should(be_false())
        end

        context "bot remains public" do
          before do
            Bot.update(shared.bot, %{description: Lorem.sentence()})
            :ok
          end

          it do:
               shared.shared_user.id
               |> HomeStream.get()
               |> is_deleted()
               |> should(be_false())

          it do:
               shared.unshared_user.id
               |> HomeStream.get()
               |> is_deleted()
               |> should(be_false())

          it do:
               user().id
               |> HomeStream.get()
               |> is_deleted()
               |> should(be_false())
        end
      end
    end

    describe "delete/1" do
      let! :result, do: Bot.delete(bot())

      it "should retun :ok" do
        result() |> should(eq :ok)
      end

      it "should remove the bot" do
        Repo.get(Bot, bot().id) |> should(be_nil())
      end

      it "should remove the bot from the full text search index" do
        TestIndexer.get_index_operations() |> should_not(be_empty())
      end
    end

    describe "owner/1" do
      subject do: Bot.owner(bot())

      it do: should(eq user())
    end

    context "subscribers" do
      before do
        sub = Factory.insert(:user)

        Bot.subscribe(bot(), sub)
        {:ok, sub: sub}
      end

      describe "subscribers/1" do
        subject do: Bot.subscribers(bot())

        it do: should(have_length 1)
        it do: should_not(have user())
      end

      describe "subscribers_query/1" do
        subject do: Bot.subscribers_query(bot()) |> Repo.all()

        it do: should(have_length 1)
        it do: should_not(have user())
        it do: subject() |> hd() |> should(be_struct User)
      end

      describe "subscriber_count/1" do
        subject do: Bot.subscriber_count(bot())
        it do: should(eq 1)
      end

      describe "notification_recipients/2" do
        let :user_jid, do: User.to_jid(user())
        let :sub_jid, do: User.to_jid(shared.sub)

        it "should not include the user" do
          result = Bot.notification_recipients(bot(), user())
          result |> should(have_length 1)
          result |> should_not(have user_jid())
          result |> should(have sub_jid())

          result = Bot.notification_recipients(bot(), shared.sub)
          result |> should(have_length 1)
          result |> should_not(have sub_jid())
          result |> should(have user_jid())
        end
      end
    end
  end

  describe "Query fragments" do
    describe "is_visible_query/2" do
      before do
        [user1, user2] = Factory.insert_list(2, :user)
        owned_bot = Factory.insert(:bot, user: user1)
        public_bot = Factory.insert(:bot, user: user2, public: true)
        shared_bot = Factory.insert(:bot, user: user2)
        Factory.insert(:share, user: user1, bot: shared_bot, sharer: user2)
        private_bot = Factory.insert(:bot, user: user2)
        pending_bot = Factory.insert(:bot, user: user1, pending: true)

        {:ok,
         user: user1,
         owned_bot: owned_bot,
         public_bot: public_bot,
         shared_bot: shared_bot,
         private_bot: private_bot,
         pending_bot: pending_bot}
      end

      it "should allow owned bots" do
        run_is_visible_query(shared.owned_bot, shared.user)
        |> should(eq shared.owned_bot)
      end

      it "should allow public bots" do
        run_is_visible_query(shared.public_bot, shared.user)
        |> should(eq shared.public_bot)
      end

      it "should allow shared bots" do
        run_is_visible_query(shared.shared_bot, shared.user)
        |> should(eq shared.shared_bot)
      end

      it "should refuse private bots" do
        run_is_visible_query(shared.private_bot, shared.user)
        |> should(be_nil())
      end
    end

    describe "discover_bots_query/2" do
      before do
        [owner, friend, follower, followee, stranger] =
          Factory.insert_list(5, :user)

        Roster.befriend(owner.id, friend.id)
        Roster.follow(follower.id, owner.id)
        Roster.follow(owner.id, followee.id)

        private_bot = Factory.insert(:bot, user: owner)
        public_bot = Factory.insert(:bot, user: owner, public: true)

        old_public_bot =
          Factory.insert(
            :bot,
            user: owner,
            public: true,
            created_at: Timestamp.from_string!("1980-01-01T00:00:00.000000")
          )

        {:ok,
         owner: owner,
         friend: friend,
         follower: follower,
         followee: followee,
         stranger: stranger,
         public_bot: public_bot,
         old_public_bot: old_public_bot}
      end

      it "should return public bots for friends" do
        run_discover_query(shared.friend)
        |> should(eq [shared.old_public_bot.id, shared.public_bot.id])
      end

      it "should return public bots for followers" do
        run_discover_query(shared.follower)
        |> should(eq [shared.old_public_bot.id, shared.public_bot.id])
      end

      it "should return nothing for followees" do
        run_discover_query(shared.followee)
        |> should(eq [])
      end

      it "should return nothing for strangers" do
        run_discover_query(shared.stranger)
        |> should(eq [])
      end

      it "should return nothing for the owner" do
        run_discover_query(shared.owner)
        |> should(eq [])
      end

      it "should limit bots to after the supplied timestamp" do
        run_discover_query(
          shared.friend,
          Timestamp.from_string!("2010-01-01T00:00:00")
        )
        |> should(eq [shared.public_bot.id])
      end
    end
  end

  defp run_is_visible_query(bot, user) do
    Bot
    |> where(id: ^bot.id)
    |> Bot.is_visible_query(user)
    |> preload(:user)
    |> Repo.one()
  end

  defp run_discover_query(user, since \\ nil) do
    user
    |> Bot.discover_query(since)
    |> Repo.all()
    |> Enum.map(& &1.id)
  end

  defp is_deleted([%HomeStreamItem{class: class}]), do: class == :deleted
end
