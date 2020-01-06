defmodule Wocky.Relation.RelationTest do
  use Wocky.DataCase, async: true

  alias Ecto.Adapters.SQL
  alias Wocky.Account.User
  alias Wocky.Contacts
  alias Wocky.GeoUtils
  alias Wocky.POI
  alias Wocky.POI.Bot
  alias Wocky.Relation
  alias Wocky.Relation.Cluster
  alias Wocky.Relation.Invitation
  alias Wocky.Repo.ID

  defp eq_bot(bot1, bot2),
    do: Map.drop(bot1, [:user]) == Map.drop(bot2, [:user])

  defp assert_bot_eq(bot1, bot2),
    do: assert(eq_bot(bot1, bot2))

  defp has_item(items, item),
    do: Enum.any?(items, &same_item(&1, item))

  defp same_item(%{id: _} = x, %{id: _} = y), do: x.id == y.id
  defp same_item(x, y), do: x == y

  defp has_bot([bot1 | _], bot2), do: eq_bot(bot1, bot2)

  defp is_empty(list), do: list == []

  setup do
    user = Factory.insert(:user)
    bot = Factory.insert(:bot, user: user)

    {:ok, user: user, bot: bot}
  end

  describe "get_bot/3" do
    setup ctx do
      pending = Factory.insert(:bot, user: ctx.user, pending: true)

      {:ok, pending: pending}
    end

    test "should return the requested bot for the owner", ctx do
      ctx.bot.id |> Relation.get_bot(ctx.user) |> assert_bot_eq(ctx.bot)
    end

    test "should return no bot for a stranger", ctx do
      stranger = Factory.insert(:user)
      refute Relation.get_bot(ctx.bot.id, stranger)
    end

    test "should return the bot for a subscriber", ctx do
      subscriber = Factory.insert(:user)
      Contacts.befriend(subscriber, ctx.user)
      Relation.subscribe(subscriber, ctx.bot)

      ctx.bot.id |> Relation.get_bot(subscriber) |> assert_bot_eq(ctx.bot)
    end

    test "should not return pending bot by default", ctx do
      refute Relation.get_bot(ctx.pending.id, ctx.user)
    end

    test "should return pending bot if explicity requested", ctx do
      ctx.pending.id
      |> Relation.get_bot(ctx.user, true)
      |> assert_bot_eq(ctx.pending)
    end
  end

  describe "get_owned_bot/3" do
    setup ctx do
      pending = Factory.insert(:bot, user: ctx.user, pending: true)

      {:ok, pending: pending}
    end

    test "should return the requested bot for the owner", ctx do
      ctx.bot.id |> Relation.get_owned_bot(ctx.user) |> assert_bot_eq(ctx.bot)
    end

    test "should return no bot for a stranger", ctx do
      stranger = Factory.insert(:user)
      refute Relation.get_owned_bot(ctx.bot.id, stranger)
    end

    test "should return no bot for a subscriber", ctx do
      subscriber = Factory.insert(:user)
      Relation.subscribe(subscriber, ctx.bot)
      refute Relation.get_owned_bot(ctx.bot.id, subscriber)
    end

    test "should not return pending bot by default", ctx do
      refute Relation.get_owned_bot(ctx.pending.id, ctx.user)
    end

    test "should return pending bot if explicity requested", ctx do
      ctx.pending.id
      |> Relation.get_owned_bot(ctx.user, true)
      |> assert_bot_eq(ctx.pending)
    end
  end

  describe "get_owned_bots/1" do
    setup ctx do
      owned_bot = Factory.insert(:bot, user: ctx.user)
      pending_bot = Factory.insert(:bot, user: ctx.user, pending: true)

      {:ok, owned_bot: owned_bot, pending_bot: pending_bot}
    end

    test "should return all owned bots", ctx do
      bots = Relation.get_owned_bots(ctx.user)

      assert length(bots) == 2
      assert has_item(bots, ctx.owned_bot)
      refute has_item(bots, ctx.pending_bot)
    end
  end

  describe "get_subscribed_bots/1" do
    setup ctx do
      pending_bot = Factory.insert(:bot, user: ctx.user, pending: true)
      subscribed_bot = Factory.insert(:bot)

      Contacts.befriend(ctx.user, subscribed_bot.user)
      Relation.subscribe(ctx.user, subscribed_bot)

      {:ok,
       owned_bot: ctx.bot,
       pending_bot: pending_bot,
       subscribed_bot: subscribed_bot}
    end

    test "should return all subscribed bots", ctx do
      subscriptions = Relation.get_subscribed_bots(ctx.user)

      assert length(subscriptions) == 2
      assert has_item(subscriptions, ctx.subscribed_bot)
      assert has_item(subscriptions, ctx.owned_bot)
      refute has_item(subscriptions, ctx.pending_bot)
    end
  end

  describe "get_active_bots/1" do
    setup ctx do
      subscriber = Factory.insert(:user)
      Contacts.befriend(subscriber, ctx.user)
      Relation.subscribe(subscriber, ctx.bot)

      {:ok, subscriber: subscriber}
    end

    test "should not return unvisted bot", ctx do
      assert Relation.get_active_bots(ctx.subscriber) == []
    end

    test "should return visted bot", ctx do
      Relation.visit(ctx.subscriber, ctx.bot, false)
      assert ctx.subscriber |> Relation.get_active_bots() |> has_item(ctx.bot)
    end

    test "should return bots with most recently visited first", ctx do
      bot2 = Factory.insert(:bot, user: ctx.user)
      Relation.subscribe(ctx.subscriber, bot2)
      Relation.visit(ctx.subscriber, ctx.bot, false)
      Relation.visit(ctx.subscriber, bot2, false)

      bot_ids =
        ctx.subscriber
        |> Relation.get_active_bots()
        |> Enum.map(& &1.id)

      assert bot_ids == [bot2.id, ctx.bot.id]
    end
  end

  describe "get_local_bots/4" do
    setup ctx do
      contact = Factory.insert(:user)

      Contacts.befriend(ctx.user, contact)

      {owned, subscribed, unrelated} =
        Enum.reduce(1..4, {[], [], []}, fn x, {o, s, u} ->
          loc = GeoUtils.point(x, x)

          owned = Factory.insert(:bot, user: ctx.user, location: loc)
          Relation.subscribe(ctx.user, owned)

          subscribed = Factory.insert(:bot, user: contact, location: loc)
          Relation.subscribe(ctx.user, subscribed)

          unrelated = Factory.insert(:bot, user: contact, location: loc)

          {[owned.id | o], [subscribed.id | s], [unrelated.id | u]}
        end)

      {:ok,
       owned: Enum.reverse(owned),
       subscribed: Enum.reverse(subscribed),
       unrelated: Enum.reverse(unrelated)}
    end

    test "should return local bots", ctx do
      assert {:ok, local_bots} =
               Relation.get_local_bots(
                 ctx.user,
                 GeoUtils.point(0.0, 0.0),
                 GeoUtils.point(5.0, 5.0),
                 10
               )

      assert length(local_bots) == 8

      ids = Enum.map(local_bots, &Map.get(&1, :id))
      assert Enum.all?(ids, &Enum.member?(ctx.owned ++ ctx.subscribed, &1))
      refute Enum.any?(ids, &Enum.member?(ctx.unrelated, &1))
    end

    test "should return local bots in a restricted area", ctx do
      [_, o | _] = ctx.owned
      [_, s | _] = ctx.subscribed

      assert {:ok, local_bots} =
               Relation.get_local_bots(
                 ctx.user,
                 GeoUtils.point(1.5, 1.5),
                 GeoUtils.point(2.5, 2.5),
                 10
               )

      assert length(local_bots) == 2

      ids = Enum.map(local_bots, &Map.get(&1, :id))
      assert Enum.all?(ids, &Enum.member?([o, s], &1))
    end

    test "should limit returned bots", ctx do
      assert {:ok, local_bots} =
               Relation.get_local_bots(
                 ctx.user,
                 GeoUtils.point(0.0, 0.0),
                 GeoUtils.point(5.0, 5.0),
                 2
               )

      assert length(local_bots) == 2

      ids = Enum.map(local_bots, &Map.get(&1, :id))
      assert ids == [List.last(ctx.subscribed), List.last(ctx.owned)]
    end

    test "should return an error when exceeding search area", ctx do
      assert {:error, :area_too_large} =
               Relation.get_local_bots(
                 ctx.user,
                 GeoUtils.point(0.0, 0.0),
                 GeoUtils.point(10.0, 10.0),
                 10
               )
    end

    test "should handle a search area straddling the 180th meridian", ctx do
      loc = GeoUtils.point(0.0, -179.0)
      bot = Factory.insert(:bot, user: ctx.user, location: loc)
      Relation.subscribe(ctx.user, bot)

      assert {:ok, local_bots} =
               Relation.get_local_bots(
                 ctx.user,
                 GeoUtils.point(1.0, 178.0),
                 GeoUtils.point(-1.0, -178.0),
                 10
               )

      assert length(local_bots) == 1
      assert hd(local_bots).id == bot.id
    end
  end

  describe "get_local_bots_clustered/5" do
    setup ctx do
      locations = [
        {1.5, 1.5},
        {0.1, 0.1},
        {0.2, 0.2},
        {0.3, 0.3},
        {0.67, 0.67},
        {0.68, 0.68},
        {3.0, 3.0}
      ]

      bots =
        Enum.map(locations, fn {lat, lon} ->
          l = GeoUtils.point(lat, lon)
          Factory.insert(:bot, location: l, user: ctx.user)
        end)

      Enum.each(bots, &Relation.subscribe(ctx.user, &1))

      {:ok, bots: bots}
    end

    test "should return bots in clusters", ctx do
      assert {:ok, bots, clusters} =
               Relation.get_local_bots_clustered(
                 ctx.user,
                 GeoUtils.point(0.0, 0.0),
                 GeoUtils.point(2.0, 2.0),
                 3,
                 3
               )

      assert length(bots) == 1
      assert hd(bots).id == hd(ctx.bots).id

      assert length(clusters) == 2

      assert has_clusters(clusters, [
               {2, 1.0, 1.0},
               {3, 0.3333333333333333, 0.3333333333333333}
             ])
    end

    defp has_clusters(clusters, expected) do
      expected
      |> Enum.map(fn {count, lat, lon} ->
        %Cluster{count: count, location: GeoUtils.point(lat, lon)}
      end)
      |> Enum.sort()
      |> Enum.zip(Enum.sort(clusters))
      |> Enum.all?(fn {x, y} -> x == y end)
    end
  end

  describe "filter_by_location/3" do
    test "finds included bot", ctx do
      a = GeoUtils.point(POI.lat(ctx.bot) - 0.1, POI.lon(ctx.bot) - 0.1)
      b = GeoUtils.point(POI.lat(ctx.bot) + 0.1, POI.lon(ctx.bot) + 0.1)

      bot =
        Bot
        |> where(id: ^ctx.bot.id)
        |> Relation.filter_by_location(a, b)
        |> Repo.one()

      assert_bot_eq(bot, ctx.bot)
    end

    test "does not find excluded bot", ctx do
      a = GeoUtils.point(POI.lat(ctx.bot) - 0.1, POI.lon(ctx.bot) - 0.1)
      b = GeoUtils.point(POI.lat(ctx.bot) - 0.2, POI.lon(ctx.bot) + 0.1)

      refute Bot
             |> where(id: ^ctx.bot.id)
             |> Relation.filter_by_location(a, b)
             |> Repo.one()
    end
  end

  describe "get_bots_by_relationship/3" do
    setup ctx do
      [stranger, subscriber, visitor, invitee] = Factory.insert_list(4, :user)
      Enum.map([subscriber, visitor, invitee], &Contacts.befriend(&1, ctx.user))

      Relation.subscribe(ctx.user, ctx.bot)
      Relation.subscribe(subscriber, ctx.bot)
      Relation.subscribe(visitor, ctx.bot)
      Relation.visit(visitor, ctx.bot, false)
      Relation.invite(invitee, ctx.bot, ctx.user)

      {:ok,
       stranger: stranger,
       subscriber: subscriber,
       visitor: visitor,
       invitee: invitee}
    end

    test "visible", ctx do
      assert ctx.user
             |> Relation.get_bots_by_relationship(:visible, ctx.user)
             |> has_bot(ctx.bot)

      assert ctx.stranger
             |> Relation.get_bots_by_relationship(:visible, ctx.user)
             |> is_empty()

      assert ctx.subscriber
             |> Relation.get_bots_by_relationship(:visible, ctx.user)
             |> has_bot(ctx.bot)

      assert ctx.visitor
             |> Relation.get_bots_by_relationship(:visible, ctx.user)
             |> has_bot(ctx.bot)

      assert ctx.invitee
             |> Relation.get_bots_by_relationship(:visible, ctx.user)
             |> has_bot(ctx.bot)
    end

    test "subscribed", ctx do
      assert ctx.user
             |> Relation.get_bots_by_relationship(:subscribed, ctx.user)
             |> has_bot(ctx.bot)

      assert ctx.stranger
             |> Relation.get_bots_by_relationship(:subscribed, ctx.user)
             |> is_empty()

      assert ctx.subscriber
             |> Relation.get_bots_by_relationship(:subscribed, ctx.user)
             |> has_bot(ctx.bot)

      assert ctx.visitor
             |> Relation.get_bots_by_relationship(:subscribed, ctx.user)
             |> has_bot(ctx.bot)

      assert ctx.invitee
             |> Relation.get_bots_by_relationship(:subscribed, ctx.user)
             |> is_empty()
    end

    test "owned", ctx do
      assert ctx.user
             |> Relation.get_bots_by_relationship(:owned, ctx.user)
             |> has_bot(ctx.bot)

      assert ctx.stranger
             |> Relation.get_bots_by_relationship(:owned, ctx.user)
             |> is_empty()

      assert ctx.subscriber
             |> Relation.get_bots_by_relationship(:owned, ctx.user)
             |> is_empty()

      assert ctx.visitor
             |> Relation.get_bots_by_relationship(:owned, ctx.user)
             |> is_empty()

      assert ctx.invitee
             |> Relation.get_bots_by_relationship(:owned, ctx.user)
             |> is_empty()
    end

    test "visiting", ctx do
      assert ctx.user
             |> Relation.get_bots_by_relationship(:visiting, ctx.user)
             |> is_empty()

      assert ctx.stranger
             |> Relation.get_bots_by_relationship(:visiting, ctx.user)
             |> is_empty()

      assert ctx.subscriber
             |> Relation.get_bots_by_relationship(:visiting, ctx.user)
             |> is_empty()

      assert ctx.visitor
             |> Relation.get_bots_by_relationship(:visiting, ctx.user)
             |> has_bot(ctx.bot)

      assert ctx.invitee
             |> Relation.get_bots_by_relationship(:visiting, ctx.user)
             |> is_empty()
    end

    test "subscribed_not_owned", ctx do
      assert ctx.user
             |> Relation.get_bots_by_relationship(
               :subscribed_not_owned,
               ctx.user
             )
             |> is_empty()

      assert ctx.stranger
             |> Relation.get_bots_by_relationship(
               :subscribed_not_owned,
               ctx.user
             )
             |> is_empty()

      assert ctx.subscriber
             |> Relation.get_bots_by_relationship(
               :subscribed_not_owned,
               ctx.user
             )
             |> has_bot(ctx.bot)

      assert ctx.visitor
             |> Relation.get_bots_by_relationship(
               :subscribed_not_owned,
               ctx.user
             )
             |> has_bot(ctx.bot)

      assert ctx.invitee
             |> Relation.get_bots_by_relationship(
               :subscribed_not_owned,
               ctx.user
             )
             |> is_empty()
    end

    test "invited", ctx do
      assert ctx.user
             |> Relation.get_bots_by_relationship(:invited, ctx.user)
             |> is_empty()

      assert ctx.stranger
             |> Relation.get_bots_by_relationship(:invited, ctx.user)
             |> is_empty()

      assert ctx.subscriber
             |> Relation.get_bots_by_relationship(:invited, ctx.user)
             |> is_empty()

      assert ctx.visitor
             |> Relation.get_bots_by_relationship(:invited, ctx.user)
             |> is_empty()

      assert ctx.invitee
             |> Relation.get_bots_by_relationship(:invited, ctx.user)
             |> has_bot(ctx.bot)
    end

    test "people to whom the bot is not visible will not get it as a result",
         ctx do
      assert ctx.user
             |> Relation.get_bots_by_relationship(:visible, ctx.stranger)
             |> is_empty()

      assert ctx.user
             |> Relation.get_bots_by_relationship(:owned, ctx.stranger)
             |> is_empty()

      assert ctx.subscriber
             |> Relation.get_bots_by_relationship(
               :subscribed,
               ctx.stranger
             )
             |> is_empty()

      assert ctx.visitor
             |> Relation.get_bots_by_relationship(:visiting, ctx.stranger)
             |> is_empty()

      assert ctx.subscriber
             |> Relation.get_bots_by_relationship(
               :subscribed_not_owned,
               ctx.stranger
             )
             |> is_empty()

      assert ctx.invitee
             |> Relation.get_bots_by_relationship(:invited, ctx.stranger)
             |> is_empty()
    end
  end

  defp run_is_visible_query(bot, user) do
    Bot
    |> where(id: ^bot.id)
    |> Relation.is_visible_query(user)
    |> preload(:user)
    |> Repo.one()
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

  describe "subscribers" do
    setup ctx do
      sub = Factory.insert(:user)
      Contacts.befriend(sub, ctx.user)
      Relation.subscribe(sub, ctx.bot)

      {:ok, sub: sub}
    end

    test "get_subscribers/1", %{bot: bot, user: user} do
      subscribers = Relation.get_subscribers(bot)

      assert length(subscribers) == 2
      assert %User{} = hd(subscribers)
      assert Enum.member?(subscribers, user)
    end

    test "get_subscriber/2", %{bot: bot, sub: sub} do
      assert bot |> Relation.get_subscriber(sub.id) |> same_item(sub)
    end

    test "get_subscriber/2 with non-subscriber", %{bot: bot} do
      refute Relation.get_subscriber(bot, ID.new())
    end
  end

  describe "get_visitors/1" do
    test "should get no visitors when none are present", ctx do
      assert Relation.get_visitors(ctx.bot) == []
    end

    test "should get visitors when they are present", ctx do
      visitor = Factory.insert(:user)
      Contacts.befriend(visitor, ctx.user)
      Relation.subscribe(visitor, ctx.bot)
      Relation.visit(visitor, ctx.bot, false)
      assert Relation.get_visitors(ctx.bot) == [visitor]
    end
  end

  describe "notification_recipients/2" do
    setup %{bot: bot, user: user} do
      friend1 = Factory.insert(:user)
      friend2 = Factory.insert(:user)
      Contacts.befriend(friend1, friend2)
      Contacts.befriend(friend1, user)
      Contacts.befriend(friend2, user)

      stranger = Factory.insert(:user)

      :ok = Relation.subscribe(friend1, bot)
      :ok = Relation.subscribe(friend2, bot)
      {:error, :permission_denied} = Relation.subscribe(stranger, bot)

      {:ok, friend1: friend1, friend2: friend2, stranger: stranger}
    end

    test "should only include friends", %{
      friend1: friend1,
      friend2: friend2,
      stranger: stranger,
      bot: bot
    } do
      recipients = Relation.notification_recipients(bot, friend1)
      assert Enum.member?(recipients, friend1)
      assert Enum.member?(recipients, friend2)
      refute Enum.member?(recipients, stranger)
    end
  end

  describe "bot relationships" do
    setup ctx do
      other_user = Factory.insert(:user)
      Contacts.befriend(ctx.user, other_user)

      owned_bot = Factory.insert(:bot, user: ctx.user)
      invited_bot = Factory.insert(:bot, user: other_user)
      subscribed_bot = Factory.insert(:bot, user: other_user)
      unaffiliated_bot = Factory.insert(:bot, user: other_user)

      Relation.invite(ctx.user, invited_bot, other_user)
      Relation.subscribe(ctx.user, subscribed_bot)

      {:ok,
       other_user: other_user,
       owned_bot: owned_bot,
       invited_bot: invited_bot,
       subscribed_bot: subscribed_bot,
       unaffiliated_bot: unaffiliated_bot}
    end

    test "owned?/2", ctx do
      assert Relation.owned?(ctx.user, ctx.owned_bot)
      refute Relation.owned?(ctx.user, ctx.invited_bot)
      refute Relation.owned?(ctx.user, ctx.subscribed_bot)
      refute Relation.owned?(ctx.user, ctx.unaffiliated_bot)
    end

    test "invited?/2", ctx do
      refute Relation.invited?(ctx.user, ctx.owned_bot)
      assert Relation.invited?(ctx.user, ctx.invited_bot)
      refute Relation.invited?(ctx.user, ctx.subscribed_bot)
      refute Relation.invited?(ctx.user, ctx.unaffiliated_bot)
    end

    test "subscribed?/2", ctx do
      refute Relation.subscribed?(ctx.user, ctx.invited_bot)
      assert Relation.subscribed?(ctx.user, ctx.subscribed_bot)
      refute Relation.subscribed?(ctx.user, ctx.unaffiliated_bot)
    end

    test "visiting?/2", ctx do
      refute Relation.visiting?(ctx.user, ctx.owned_bot)
      refute Relation.visiting?(ctx.user, ctx.invited_bot)
      refute Relation.visiting?(ctx.user, ctx.subscribed_bot)
      refute Relation.visiting?(ctx.user, ctx.unaffiliated_bot)

      Relation.visit(ctx.user, ctx.subscribed_bot)
      assert Relation.visiting?(ctx.user, ctx.subscribed_bot)
    end

    test "visible?/2", ctx do
      assert Relation.visible?(ctx.user, ctx.owned_bot)
      assert Relation.visible?(ctx.user, ctx.invited_bot)
      assert Relation.visible?(ctx.user, ctx.subscribed_bot)
      refute Relation.visible?(ctx.user, ctx.unaffiliated_bot)
    end

    test "get_bot_relationships/2", ctx do
      rels! = Relation.get_bot_relationships(ctx.user, ctx.owned_bot)

      assert length(rels!) == 3
      assert Enum.member?(rels!, :visible)
      assert Enum.member?(rels!, :owned)
      assert Enum.member?(rels!, :subscribed)

      rels! = Relation.get_bot_relationships(ctx.user, ctx.invited_bot)

      assert length(rels!) == 2
      assert Enum.member?(rels!, :visible)
      assert Enum.member?(rels!, :invited)

      rels! = Relation.get_bot_relationships(ctx.user, ctx.subscribed_bot)

      assert length(rels!) == 2
      assert Enum.member?(rels!, :visible)
      assert Enum.member?(rels!, :subscribed)
    end
  end

  # -------------------------------------------------------------------
  # Subsriptions

  defp create_subscription(ctx) do
    [user, visitor, stranger] = Factory.insert_list(3, :user)
    Contacts.befriend(ctx.user, user)
    Contacts.befriend(ctx.user, visitor)

    Factory.insert(:subscription, user: user, bot: ctx.bot)

    Factory.insert(
      :subscription,
      user: visitor,
      bot: ctx.bot,
      visitor: true
    )

    {:ok, owner: ctx.user, user: user, visitor: visitor, stranger: stranger}
  end

  describe "get_subscription/2" do
    setup :create_subscription

    test "should return the subscription", ctx do
      assert Relation.get_subscription(ctx.user, ctx.bot)
    end

    test "should return nil when the user does not exist", ctx do
      user = Factory.build(:user)
      refute Relation.get_subscription(user, ctx.bot)
    end

    test "should return nil when the bot does not exist", ctx do
      bot = Factory.build(:bot)
      refute Relation.get_subscription(ctx.user, bot)
    end

    test "should return nil when the user is not subscribed to the bot", ctx do
      refute Relation.get_subscription(ctx.stranger, ctx.bot)
    end
  end

  describe "subscribe/2" do
    setup :create_subscription

    test "when a subscription does not already exist", ctx do
      new_user = Factory.insert(:user)
      Contacts.befriend(new_user, ctx.owner)

      result = Relation.subscribe(new_user, ctx.bot)

      assert result == :ok
      assert Relation.subscribed?(new_user, ctx.bot)
    end

    test "when a subscription already exists", ctx do
      result = Relation.subscribe(ctx.visitor, ctx.bot)

      assert result == :ok
      assert Relation.visiting?(ctx.visitor, ctx.bot)
    end
  end

  describe "unsubscribe/2" do
    setup :create_subscription

    test "when a subscription exists", ctx do
      result = Relation.unsubscribe(ctx.user, ctx.bot)

      assert result == :ok
      refute Relation.subscribed?(ctx.user, ctx.bot)
    end

    test "when the bot owner is trying to unsubscribe", ctx do
      assert {:error, _} = Relation.unsubscribe(ctx.owner, ctx.bot)
    end

    test "when the user doesn't exist", ctx do
      user = Factory.build(:user)

      assert Relation.unsubscribe(user, ctx.bot) == :ok
    end

    test "when the bot doesn't exist", ctx do
      bot = Factory.build(:bot)

      assert Relation.unsubscribe(ctx.user, bot) == :ok
    end
  end

  describe "visit/2" do
    setup :create_subscription

    test "should set the subscriber as a visitor", ctx do
      assert Relation.visit(ctx.user, ctx.bot) == :ok
      assert Relation.visiting?(ctx.user, ctx.bot)
    end
  end

  describe "depart/2" do
    setup :create_subscription

    test "should set the visitor as a subscriber", ctx do
      assert Relation.depart(ctx.visitor, ctx.bot) == :ok
      refute Relation.visiting?(ctx.visitor, ctx.bot)
    end
  end

  describe "delete_subscriptions_for_owned_bots/2" do
    setup :create_subscription

    test "should delete subscriptions to bots owned by the specified user",
         ctx do
      assert :ok ==
               Relation.delete_subscriptions_for_owned_bots(
                 ctx.owner,
                 ctx.user
               )

      refute Relation.subscribed?(ctx.user, ctx.bot)
    end

    test "should not delete subscriptions to bots owned by another user", ctx do
      u = Factory.insert(:user)
      assert :ok == Relation.delete_subscriptions_for_owned_bots(u, ctx.user)
      assert Relation.subscribed?(ctx.user, ctx.bot)
    end
  end

  defp is_subscribed_sp(user, bot) do
    {:ok, u} = Ecto.UUID.dump(user.id)
    {:ok, b} = Ecto.UUID.dump(bot.id)

    Repo
    |> SQL.query!("SELECT is_subscribed($1, $2)", [u, b])
    |> Map.get(:rows)
    |> hd
    |> hd
  end

  describe "is_subscribed/2 stored procedure" do
    setup :create_subscription

    test "should return true if the user is subscribed to the bot", ctx do
      assert is_subscribed_sp(ctx.user, ctx.bot)
    end

    test "should return false when the user does not exist", ctx do
      user = Factory.build(:user, device: "testing")
      refute is_subscribed_sp(user, ctx.bot)
    end

    test "should return false when the bot does not exist", ctx do
      bot = Factory.build(:bot)
      refute is_subscribed_sp(ctx.user, bot)
    end

    test "should return false when the user is not subscribed to the bot",
         ctx do
      refute is_subscribed_sp(ctx.stranger, ctx.bot)
    end
  end

  # -------------------------------------------------------------------
  # Invitations

  describe "invite/3" do
    setup ctx do
      [invitee, stranger] = Factory.insert_list(2, :user, device: "testing")
      Contacts.befriend(ctx.user, invitee)

      {:ok, invitee: invitee, stranger: stranger}
    end

    test "should create an invitation", ctx do
      assert {:ok, invitation} = Relation.invite(ctx.invitee, ctx.bot, ctx.user)

      assert invitation == Repo.get_by(Invitation, id: invitation.id)
      refute invitation.accepted
    end

    test "refuse invitation to non-owned bot", ctx do
      other_user = Factory.insert(:user)

      assert {:error, :permission_denied} ==
               Relation.invite(ctx.invitee, ctx.bot, other_user)
    end

    test "refuse invitation to non-friend", ctx do
      assert {:error, :permission_denied} =
               Relation.invite(ctx.stranger, ctx.bot, ctx.user)
    end

    test "subsequent invitations should overwrite existing ones", ctx do
      assert {:ok, invitation} = Relation.invite(ctx.invitee, ctx.bot, ctx.user)

      assert {:ok, invitation2} =
               Relation.invite(ctx.invitee, ctx.bot, ctx.user)

      assert invitation.id == invitation2.id

      assert DateTime.compare(invitation.updated_at, invitation2.updated_at) ==
               :lt
    end
  end

  defp setup_invitation(ctx) do
    invitee = Factory.insert(:user, device: "testing")
    Contacts.befriend(ctx.user, invitee)

    invitation =
      Factory.insert(:bot_invitation,
        user: ctx.user,
        invitee: invitee,
        bot: ctx.bot
      )

    {:ok, invitee: invitee, invitation: invitation}
  end

  describe "get_invitation/2 by id" do
    setup :setup_invitation

    test "Invitee can get their own invitation", ctx do
      assert %Invitation{} =
               Relation.get_invitation(ctx.invitation.id, ctx.invitee)
    end

    test "Unrelated user cannot get the invitation", ctx do
      refute Relation.get_invitation(ctx.invitation.id, Factory.insert(:user))
    end
  end

  describe "respond/3" do
    setup :setup_invitation

    test "Invitee can accept", ctx do
      assert {:ok, invitation} =
               Relation.respond(ctx.invitation, true, ctx.invitee)

      assert invitation.accepted == true
    end

    test "Invitee becomes subscribed if they accept", ctx do
      refute Relation.subscribed?(ctx.invitee, ctx.bot)

      assert {:ok, invitation} =
               Relation.respond(ctx.invitation, true, ctx.invitee)

      assert Relation.subscribed?(ctx.invitee, ctx.bot)
    end

    test "Invitee can decline", ctx do
      assert {:ok, invitation} =
               Relation.respond(ctx.invitation, false, ctx.invitee)

      assert invitation.accepted == false
    end

    test "Invitee does not become subscribed if they decline", ctx do
      refute Relation.subscribed?(ctx.invitee, ctx.bot)

      assert {:ok, invitation} =
               Relation.respond(ctx.invitation, false, ctx.invitee)

      refute Relation.subscribed?(ctx.invitee, ctx.bot)
    end

    test "Inviter cannot respond", ctx do
      assert {:error, :permission_denied} =
               Relation.respond(ctx.invitation, false, ctx.user)
    end

    test "Other user cannot respond", ctx do
      assert {:error, :permission_denied} =
               Relation.respond(
                 ctx.invitation,
                 false,
                 Factory.insert(:user)
               )
    end
  end

  describe "delete_invitation/2" do
    setup :setup_invitation

    setup %{user: user, invitee: invitee} do
      invitation = Factory.insert(:bot_invitation, user: user, invitee: invitee)
      invitation2 = Factory.insert(:bot_invitation, invitee: invitee)

      Relation.delete_invitation(user, invitee)

      {:ok, invitation: invitation, invitation2: invitation2}
    end

    test "it should delete the invitation between the users", ctx do
      refute Repo.get(Invitation, ctx.invitation.id)
    end

    test "it should not delete other invitations to the user", ctx do
      assert Repo.get(Invitation, ctx.invitation2.id)
    end
  end
end
