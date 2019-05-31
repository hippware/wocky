defmodule Wocky.Bot.BotTest do
  use Wocky.DataCase, async: true

  alias Wocky.Account.User
  alias Wocky.Bot
  alias Wocky.GeoUtils
  alias Wocky.Repo
  alias Wocky.Repo.Factory
  alias Wocky.Repo.ID
  alias Wocky.Roster

  setup do
    user = Factory.insert(:user)
    bot = Factory.insert(:bot, user: user)

    {:ok, user: user, bot: bot}
  end

  describe "validations" do
    setup do
      {:ok,
       attrs: %{
         id: ID.new(),
         user_id: ID.new(),
         title: "test bot",
         location: GeoUtils.point(5.0, 5.0)
       }}
    end

    test "should pass with valid attributes", %{attrs: attrs} do
      assert Bot.changeset(%Bot{}, attrs).valid?
    end

    test "should set pending to 'false'", %{attrs: attrs} do
      changeset = Bot.changeset(%Bot{}, attrs)
      refute changeset.changes.pending
    end

    test "should fail with missing fields", %{attrs: attrs} do
      changeset = Bot.changeset(%Bot{}, %{})

      refute changeset.valid?

      errors = Map.keys(errors_on(changeset))

      for {attr, _} <- attrs do
        assert Enum.member?(errors, attr)
      end
    end

    test "should fail with negative radius", %{attrs: attrs} do
      changeset = Bot.changeset(%Bot{}, Map.put(attrs, :radius, -1))
      refute changeset.valid?
      assert errors_on(changeset).radius
    end

    test "should fail with a nil description", %{attrs: attrs} do
      changeset = Bot.changeset(%Bot{}, Map.put(attrs, :description, nil))
      refute changeset.valid?
      assert errors_on(changeset).description
    end
  end

  describe "get/2" do
    setup ctx do
      pending = Factory.insert(:bot, user: ctx.user, pending: true)

      {:ok, pending: pending}
    end

    test "should return the requested bot", %{bot: bot} do
      assert bot.id |> Bot.get() |> Repo.preload(:user) == bot
    end

    test "should return nil for non-existant bots" do
      refute Bot.get(ID.new())
    end

    test "should not return pending bots by default", %{pending: pending} do
      refute Bot.get(pending.id)
    end

    test "should return pending bots if specified", %{pending: pending} do
      assert pending.id |> Bot.get(true) |> Repo.preload(:user) == pending
    end
  end

  describe "get_bot/3" do
    setup ctx do
      pending = Factory.insert(:bot, user: ctx.user, pending: true)

      {:ok, pending: pending}
    end

    test "should return the requested bot for the owner",
         %{bot: bot, user: user} do
      assert bot.id |> Bot.get_bot(user) |> Repo.preload(:user) == bot
    end

    test "should return no bot for a stranger",
         %{bot: bot} do
      stranger = Factory.insert(:user)
      assert bot.id |> Bot.get_bot(stranger) == nil
    end

    test "should return the bot for a subscriber", ctx do
      subscriber = Factory.insert(:user)
      Roster.befriend(subscriber, ctx.user)
      Bot.subscribe(ctx.bot, subscriber)

      assert ctx.bot.id |> Bot.get_bot(subscriber) |> Repo.preload(:user) ==
               ctx.bot
    end

    test "should not return pending bot by default",
         %{pending: pending, user: user} do
      assert pending.id |> Bot.get_bot(user) == nil
    end

    test "should return pending bot if explicity requested",
         %{pending: pending, user: user} do
      assert pending.id |> Bot.get_bot(user, true) |> Repo.preload(:user) ==
               pending
    end
  end

  describe "get_owned_bot/3" do
    setup ctx do
      pending = Factory.insert(:bot, user: ctx.user, pending: true)

      {:ok, pending: pending}
    end

    test "should return the requested bot for the owner",
         %{bot: bot, user: user} do
      assert bot.id |> Bot.get_owned_bot(user) |> Repo.preload(:user) == bot
    end

    test "should return no bot for a stranger",
         %{bot: bot} do
      stranger = Factory.insert(:user)
      assert bot.id |> Bot.get_owned_bot(stranger) == nil
    end

    test "should return no bot for a subscriber", %{bot: bot} do
      subscriber = Factory.insert(:user)
      Bot.subscribe(bot, subscriber)
      assert bot.id |> Bot.get_owned_bot(subscriber) == nil
    end

    test "should not return pending bot by default",
         %{pending: pending, user: user} do
      assert pending.id |> Bot.get_owned_bot(user) == nil
    end

    test "should return pending bot if explicity requested",
         %{pending: pending, user: user} do
      assert pending.id |> Bot.get_owned_bot(user, true) |> Repo.preload(:user) ==
               pending
    end
  end

  describe "by_relationship_query/3" do
    setup ctx do
      [stranger, subscriber, visitor, invitee] = Factory.insert_list(4, :user)
      Enum.map([subscriber, visitor, invitee], &Roster.befriend(&1, ctx.user))

      Bot.subscribe(ctx.bot, ctx.user)
      Bot.subscribe(ctx.bot, subscriber)
      Bot.subscribe(ctx.bot, visitor)
      Bot.visit(ctx.bot, visitor, false)
      Bot.Invitation.put(invitee, ctx.bot, ctx.user)

      {:ok,
       stranger: stranger,
       subscriber: subscriber,
       visitor: visitor,
       invitee: invitee}
    end

    test "visible", ctx do
      assert ctx.user
             |> Bot.by_relationship_query(:visible, ctx.user)
             |> has_bot(ctx.bot)

      assert ctx.stranger
             |> Bot.by_relationship_query(:visible, ctx.user)
             |> is_empty()

      assert ctx.subscriber
             |> Bot.by_relationship_query(:visible, ctx.user)
             |> has_bot(ctx.bot)

      assert ctx.visitor
             |> Bot.by_relationship_query(:visible, ctx.user)
             |> has_bot(ctx.bot)

      assert ctx.invitee
             |> Bot.by_relationship_query(:visible, ctx.user)
             |> has_bot(ctx.bot)
    end

    test "subscribed", ctx do
      assert ctx.user
             |> Bot.by_relationship_query(:subscribed, ctx.user)
             |> has_bot(ctx.bot)

      assert ctx.stranger
             |> Bot.by_relationship_query(:subscribed, ctx.user)
             |> is_empty()

      assert ctx.subscriber
             |> Bot.by_relationship_query(:subscribed, ctx.user)
             |> has_bot(ctx.bot)

      assert ctx.visitor
             |> Bot.by_relationship_query(:subscribed, ctx.user)
             |> has_bot(ctx.bot)

      assert ctx.invitee
             |> Bot.by_relationship_query(:subscribed, ctx.user)
             |> is_empty()
    end

    test "owned", ctx do
      assert ctx.user
             |> Bot.by_relationship_query(:owned, ctx.user)
             |> has_bot(ctx.bot)

      assert ctx.stranger
             |> Bot.by_relationship_query(:owned, ctx.user)
             |> is_empty()

      assert ctx.subscriber
             |> Bot.by_relationship_query(:owned, ctx.user)
             |> is_empty()

      assert ctx.visitor
             |> Bot.by_relationship_query(:owned, ctx.user)
             |> is_empty()

      assert ctx.invitee
             |> Bot.by_relationship_query(:owned, ctx.user)
             |> is_empty()
    end

    test "visiting", ctx do
      assert ctx.user
             |> Bot.by_relationship_query(:visiting, ctx.user)
             |> is_empty()

      assert ctx.stranger
             |> Bot.by_relationship_query(:visiting, ctx.user)
             |> is_empty()

      assert ctx.subscriber
             |> Bot.by_relationship_query(:visiting, ctx.user)
             |> is_empty()

      assert ctx.visitor
             |> Bot.by_relationship_query(:visiting, ctx.user)
             |> has_bot(ctx.bot)

      assert ctx.invitee
             |> Bot.by_relationship_query(:visiting, ctx.user)
             |> is_empty()
    end

    test "subscribed_not_owned", ctx do
      assert ctx.user
             |> Bot.by_relationship_query(
               :subscribed_not_owned,
               ctx.user
             )
             |> is_empty()

      assert ctx.stranger
             |> Bot.by_relationship_query(
               :subscribed_not_owned,
               ctx.user
             )
             |> is_empty()

      assert ctx.subscriber
             |> Bot.by_relationship_query(
               :subscribed_not_owned,
               ctx.user
             )
             |> has_bot(ctx.bot)

      assert ctx.visitor
             |> Bot.by_relationship_query(
               :subscribed_not_owned,
               ctx.user
             )
             |> has_bot(ctx.bot)

      assert ctx.invitee
             |> Bot.by_relationship_query(
               :subscribed_not_owned,
               ctx.user
             )
             |> is_empty()
    end

    test "invited", ctx do
      assert ctx.user
             |> Bot.by_relationship_query(:invited, ctx.user)
             |> is_empty()

      assert ctx.stranger
             |> Bot.by_relationship_query(:invited, ctx.user)
             |> is_empty()

      assert ctx.subscriber
             |> Bot.by_relationship_query(:invited, ctx.user)
             |> is_empty()

      assert ctx.visitor
             |> Bot.by_relationship_query(:invited, ctx.user)
             |> is_empty()

      assert ctx.invitee
             |> Bot.by_relationship_query(:invited, ctx.user)
             |> has_bot(ctx.bot)
    end

    test "people to whom the bot is not visible will not get it as a result",
         ctx do
      assert ctx.user
             |> Bot.by_relationship_query(:visible, ctx.stranger)
             |> is_empty()

      assert ctx.user
             |> Bot.by_relationship_query(:owned, ctx.stranger)
             |> is_empty()

      assert ctx.subscriber
             |> Bot.by_relationship_query(
               :subscribed,
               ctx.stranger
             )
             |> is_empty()

      assert ctx.visitor
             |> Bot.by_relationship_query(:visiting, ctx.stranger)
             |> is_empty()

      assert ctx.subscriber
             |> Bot.by_relationship_query(
               :subscribed_not_owned,
               ctx.stranger
             )
             |> is_empty()

      assert ctx.invitee
             |> Bot.by_relationship_query(:invited, ctx.stranger)
             |> is_empty()
    end
  end

  describe "active_bots_query/1" do
    setup ctx do
      subscriber = Factory.insert(:user)
      Roster.befriend(subscriber, ctx.user)
      Bot.subscribe(ctx.bot, subscriber)

      {:ok, subscriber: subscriber}
    end

    test "should not return unvisted bot", ctx do
      assert ctx.subscriber |> Bot.active_bots_query() |> is_empty()
    end

    test "should return visted bot", ctx do
      Bot.visit(ctx.bot, ctx.subscriber, false)
      assert ctx.subscriber |> Bot.active_bots_query() |> has_bot(ctx.bot)
    end

    test "should return bots with most recently visited first", ctx do
      bot2 = Factory.insert(:bot, user: ctx.user)
      Bot.subscribe(bot2, ctx.subscriber)
      Bot.visit(ctx.bot, ctx.subscriber, false)
      Bot.visit(bot2, ctx.subscriber, false)

      bot_ids =
        ctx.subscriber
        |> Bot.active_bots_query()
        |> Repo.all()
        |> Enum.map(& &1.id)

      assert bot_ids == [bot2.id, ctx.bot.id]
    end
  end

  describe "preallocate/2" do
    setup ctx do
      preallocated = Bot.preallocate(ctx.user)

      {:ok, preallocated: preallocated}
    end

    test "returns a pending bot", %{preallocated: preallocated} do
      assert preallocated.pending
    end

    test "creates a bot in the database", %{preallocated: preallocated} do
      db_bot = Repo.get(Bot, preallocated.id)

      assert db_bot.user_id == preallocated.user_id
    end

    test "raises on error" do
      assert_raise Ecto.InvalidChangesetError, fn ->
        Bot.preallocate(Factory.build(:user))
      end
    end
  end

  describe "insert/1" do
    test "returns an ok result on success", %{user: user} do
      bot_params = Factory.params_for(:bot, user: user)

      assert {:ok, _} = Bot.insert(bot_params, user)
    end

    test "returns an error result on failure", %{user: user} do
      assert {:error, _} = Bot.insert(%{}, user)
    end
  end

  describe "update/2" do
    test "returns an ok result on success", %{bot: bot} do
      assert {:ok, _} = Bot.update(bot, %{title: "updated bot"})
    end

    test "returns an error result on failure" do
      assert {:error, _} = Bot.update(%Bot{}, %{})
    end

    test "should normalize latitude and longitude", %{bot: bot} do
      {:ok, %Bot{id: id}} =
        Bot.update(bot, %{location: GeoUtils.point(-95.0, -185)})

      assert Repo.get(Bot, id).location == GeoUtils.point(-85, 175)
    end
  end

  describe "delete/1" do
    setup ctx do
      result = Bot.delete(ctx.bot)

      {:ok, result: result}
    end

    test "should retun :ok", %{result: result} do
      assert result == :ok
    end

    test "should remove the bot", %{bot: bot} do
      refute Repo.get(Bot, bot.id)
    end
  end

  describe "subscribers" do
    setup ctx do
      sub = Factory.insert(:user)
      Roster.befriend(sub, ctx.user)
      Bot.subscribe(ctx.bot, sub)

      {:ok, sub: sub}
    end

    test "subscribers_query/1", %{bot: bot, user: user} do
      subscribers = bot |> Bot.subscribers_query() |> Repo.all()

      assert length(subscribers) == 1
      assert %User{} = hd(subscribers)
      refute Enum.member?(subscribers, user)
    end

    test "subscriber_query/2", %{bot: bot, sub: sub} do
      subscriber = bot |> Bot.subscriber_query(sub.id) |> Repo.one()

      assert subscriber.id == sub.id
    end

    test "subscriber_query/2 with non-subscriber", %{bot: bot} do
      assert bot |> Bot.subscriber_query(ID.new()) |> Repo.one() == nil
    end
  end

  describe "visitors_query/1" do
    test "should get no visitors when none are present", ctx do
      assert ctx.bot |> Bot.visitors_query() |> Repo.one() == nil
    end

    test "should get visitors when they are present", ctx do
      visitor = Factory.insert(:user)
      Roster.befriend(visitor, ctx.user)
      Bot.subscribe(ctx.bot, visitor)
      Bot.visit(ctx.bot, visitor, false)
      assert ctx.bot |> Bot.visitors_query() |> Repo.one() == visitor
    end
  end

  describe "is_visible_query/2" do
    setup do
      [user1, user2] = Factory.insert_list(2, :user)
      owned_bot = Factory.insert(:bot, user: user1)
      invited_bot = Factory.insert(:bot, user: user2)

      Factory.insert(:bot_invitation,
        invitee: user1,
        bot: invited_bot,
        user: user2
      )

      private_bot = Factory.insert(:bot, user: user2)
      pending_bot = Factory.insert(:bot, user: user1, pending: true)
      subscribed_bot = Factory.insert(:bot, user: user2)
      Factory.insert(:subscription, bot: subscribed_bot, user: user1)

      {:ok,
       user: user1,
       owned_bot: owned_bot,
       invited_bot: invited_bot,
       private_bot: private_bot,
       pending_bot: pending_bot,
       subscribed_bot: subscribed_bot}
    end

    test "should allow owned bots", ctx do
      assert run_is_visible_query(ctx.owned_bot, ctx.user) == ctx.owned_bot
    end

    test "should allow invited bots", ctx do
      assert run_is_visible_query(ctx.invited_bot, ctx.user) == ctx.invited_bot
    end

    test "should refuse private bots", ctx do
      refute run_is_visible_query(ctx.private_bot, ctx.user)
    end

    test "should allow subscribed bots", ctx do
      assert run_is_visible_query(ctx.subscribed_bot, ctx.user) ==
               ctx.subscribed_bot
    end
  end

  describe "filter_by_location/3" do
    test "finds included bot", ctx do
      a = GeoUtils.point(Bot.lat(ctx.bot) - 0.1, Bot.lon(ctx.bot) - 0.1)
      b = GeoUtils.point(Bot.lat(ctx.bot) + 0.1, Bot.lon(ctx.bot) + 0.1)

      assert Bot
             |> where(id: ^ctx.bot.id)
             |> Bot.filter_by_location(a, b)
             |> Repo.one()
             |> Repo.preload(:user) == ctx.bot
    end

    test "does not find excluded bot", ctx do
      a = GeoUtils.point(Bot.lat(ctx.bot) - 0.1, Bot.lon(ctx.bot) - 0.1)
      b = GeoUtils.point(Bot.lat(ctx.bot) - 0.2, Bot.lon(ctx.bot) + 0.1)

      assert Bot
             |> where(id: ^ctx.bot.id)
             |> Bot.filter_by_location(a, b)
             |> Repo.one() == nil
    end
  end

  describe "notification_recipients/2" do
    setup %{bot: bot, user: user} do
      friend1 = Factory.insert(:user)
      friend2 = Factory.insert(:user)
      Roster.befriend(friend1, friend2)
      Roster.befriend(friend1, user)
      Roster.befriend(friend2, user)

      stranger = Factory.insert(:user)

      :ok = Bot.subscribe(bot, friend1)
      :ok = Bot.subscribe(bot, friend2)
      {:error, :permission_denied} = Bot.subscribe(bot, stranger)

      {:ok, friend1: friend1, friend2: friend2, stranger: stranger}
    end

    test "should only include friends", %{
      friend1: friend1,
      friend2: friend2,
      stranger: stranger,
      bot: bot
    } do
      recipients = Bot.notification_recipients(bot, friend1)
      assert Enum.member?(recipients, friend1)
      assert Enum.member?(recipients, friend2)
      refute Enum.member?(recipients, stranger)
    end
  end

  # Query helper functions

  defp run_is_visible_query(bot, user) do
    Bot
    |> where(id: ^bot.id)
    |> Bot.is_visible_query(user)
    |> preload(:user)
    |> Repo.one()
  end

  defp has_bot(query, bot),
    do: query |> Repo.one() |> Repo.preload(:user) == bot

  defp is_empty(query),
    do: query |> Repo.one() == nil
end
