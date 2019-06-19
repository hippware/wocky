defmodule Wocky.Relations.RelationsTest do
  use Wocky.DataCase, async: true

  alias Ecto.Adapters.SQL
  alias Wocky.Account.User
  alias Wocky.Bots
  alias Wocky.Bots.Bot
  alias Wocky.GeoUtils
  alias Wocky.Relations
  alias Wocky.Relations.Invitation
  alias Wocky.Repo.Factory
  alias Wocky.Repo.ID
  alias Wocky.Roster

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
      ctx.bot.id |> Relations.get_bot(ctx.user) |> assert_bot_eq(ctx.bot)
    end

    test "should return no bot for a stranger", ctx do
      stranger = Factory.insert(:user)
      refute Relations.get_bot(ctx.bot.id, stranger)
    end

    test "should return the bot for a subscriber", ctx do
      subscriber = Factory.insert(:user)
      Roster.befriend(subscriber, ctx.user)
      Relations.subscribe(subscriber, ctx.bot)

      ctx.bot.id |> Relations.get_bot(subscriber) |> assert_bot_eq(ctx.bot)
    end

    test "should not return pending bot by default", ctx do
      refute Relations.get_bot(ctx.pending.id, ctx.user)
    end

    test "should return pending bot if explicity requested", ctx do
      ctx.pending.id
      |> Relations.get_bot(ctx.user, true)
      |> assert_bot_eq(ctx.pending)
    end
  end

  describe "get_owned_bot/3" do
    setup ctx do
      pending = Factory.insert(:bot, user: ctx.user, pending: true)

      {:ok, pending: pending}
    end

    test "should return the requested bot for the owner", ctx do
      ctx.bot.id |> Relations.get_owned_bot(ctx.user) |> assert_bot_eq(ctx.bot)
    end

    test "should return no bot for a stranger", ctx do
      stranger = Factory.insert(:user)
      refute Relations.get_owned_bot(ctx.bot.id, stranger)
    end

    test "should return no bot for a subscriber", ctx do
      subscriber = Factory.insert(:user)
      Relations.subscribe(subscriber, ctx.bot)
      refute Relations.get_owned_bot(ctx.bot.id, subscriber)
    end

    test "should not return pending bot by default", ctx do
      refute Relations.get_owned_bot(ctx.pending.id, ctx.user)
    end

    test "should return pending bot if explicity requested", ctx do
      ctx.pending.id
      |> Relations.get_owned_bot(ctx.user, true)
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
      bots = Relations.get_owned_bots(ctx.user)

      assert length(bots) == 2
      assert has_item(bots, ctx.owned_bot)
      refute has_item(bots, ctx.pending_bot)
    end
  end

  describe "get_subscribed_bots/1" do
    setup ctx do
      pending_bot = Factory.insert(:bot, user: ctx.user, pending: true)
      subscribed_bot = Factory.insert(:bot)

      Roster.befriend(ctx.user, subscribed_bot.user)
      Relations.subscribe(ctx.user, subscribed_bot)

      {:ok,
       owned_bot: ctx.bot,
       pending_bot: pending_bot,
       subscribed_bot: subscribed_bot}
    end

    test "should return all subscribed bots", ctx do
      subscriptions = Relations.get_subscribed_bots(ctx.user)

      assert length(subscriptions) == 1
      assert has_item(subscriptions, ctx.subscribed_bot)
      refute has_item(subscriptions, ctx.owned_bot)
      refute has_item(subscriptions, ctx.pending_bot)
    end
  end

  describe "get_active_bots/1" do
    setup ctx do
      subscriber = Factory.insert(:user)
      Roster.befriend(subscriber, ctx.user)
      Relations.subscribe(subscriber, ctx.bot)

      {:ok, subscriber: subscriber}
    end

    test "should not return unvisted bot", ctx do
      assert Relations.get_active_bots(ctx.subscriber) == []
    end

    test "should return visted bot", ctx do
      Relations.visit(ctx.subscriber, ctx.bot, false)
      assert ctx.subscriber |> Relations.get_active_bots() |> has_item(ctx.bot)
    end

    test "should return bots with most recently visited first", ctx do
      bot2 = Factory.insert(:bot, user: ctx.user)
      Relations.subscribe(ctx.subscriber, bot2)
      Relations.visit(ctx.subscriber, ctx.bot, false)
      Relations.visit(ctx.subscriber, bot2, false)

      bot_ids =
        ctx.subscriber
        |> Relations.get_active_bots()
        |> Enum.map(& &1.id)

      assert bot_ids == [bot2.id, ctx.bot.id]
    end
  end

  describe "filter_by_location/3" do
    test "finds included bot", ctx do
      a = GeoUtils.point(Bots.lat(ctx.bot) - 0.1, Bots.lon(ctx.bot) - 0.1)
      b = GeoUtils.point(Bots.lat(ctx.bot) + 0.1, Bots.lon(ctx.bot) + 0.1)

      bot =
        Bot
        |> where(id: ^ctx.bot.id)
        |> Relations.filter_by_location(a, b)
        |> Repo.one()

      assert_bot_eq(bot, ctx.bot)
    end

    test "does not find excluded bot", ctx do
      a = GeoUtils.point(Bots.lat(ctx.bot) - 0.1, Bots.lon(ctx.bot) - 0.1)
      b = GeoUtils.point(Bots.lat(ctx.bot) - 0.2, Bots.lon(ctx.bot) + 0.1)

      refute Bot
             |> where(id: ^ctx.bot.id)
             |> Relations.filter_by_location(a, b)
             |> Repo.one()
    end
  end

  describe "get_bots_by_relationship/3" do
    setup ctx do
      [stranger, subscriber, visitor, invitee] = Factory.insert_list(4, :user)
      Enum.map([subscriber, visitor, invitee], &Roster.befriend(&1, ctx.user))

      Relations.subscribe(ctx.user, ctx.bot)
      Relations.subscribe(subscriber, ctx.bot)
      Relations.subscribe(visitor, ctx.bot)
      Relations.visit(visitor, ctx.bot, false)
      Relations.invite(invitee, ctx.bot, ctx.user)

      {:ok,
       stranger: stranger,
       subscriber: subscriber,
       visitor: visitor,
       invitee: invitee}
    end

    test "visible", ctx do
      assert ctx.user
             |> Relations.get_bots_by_relationship(:visible, ctx.user)
             |> has_bot(ctx.bot)

      assert ctx.stranger
             |> Relations.get_bots_by_relationship(:visible, ctx.user)
             |> is_empty()

      assert ctx.subscriber
             |> Relations.get_bots_by_relationship(:visible, ctx.user)
             |> has_bot(ctx.bot)

      assert ctx.visitor
             |> Relations.get_bots_by_relationship(:visible, ctx.user)
             |> has_bot(ctx.bot)

      assert ctx.invitee
             |> Relations.get_bots_by_relationship(:visible, ctx.user)
             |> has_bot(ctx.bot)
    end

    test "subscribed", ctx do
      assert ctx.user
             |> Relations.get_bots_by_relationship(:subscribed, ctx.user)
             |> has_bot(ctx.bot)

      assert ctx.stranger
             |> Relations.get_bots_by_relationship(:subscribed, ctx.user)
             |> is_empty()

      assert ctx.subscriber
             |> Relations.get_bots_by_relationship(:subscribed, ctx.user)
             |> has_bot(ctx.bot)

      assert ctx.visitor
             |> Relations.get_bots_by_relationship(:subscribed, ctx.user)
             |> has_bot(ctx.bot)

      assert ctx.invitee
             |> Relations.get_bots_by_relationship(:subscribed, ctx.user)
             |> is_empty()
    end

    test "owned", ctx do
      assert ctx.user
             |> Relations.get_bots_by_relationship(:owned, ctx.user)
             |> has_bot(ctx.bot)

      assert ctx.stranger
             |> Relations.get_bots_by_relationship(:owned, ctx.user)
             |> is_empty()

      assert ctx.subscriber
             |> Relations.get_bots_by_relationship(:owned, ctx.user)
             |> is_empty()

      assert ctx.visitor
             |> Relations.get_bots_by_relationship(:owned, ctx.user)
             |> is_empty()

      assert ctx.invitee
             |> Relations.get_bots_by_relationship(:owned, ctx.user)
             |> is_empty()
    end

    test "visiting", ctx do
      assert ctx.user
             |> Relations.get_bots_by_relationship(:visiting, ctx.user)
             |> is_empty()

      assert ctx.stranger
             |> Relations.get_bots_by_relationship(:visiting, ctx.user)
             |> is_empty()

      assert ctx.subscriber
             |> Relations.get_bots_by_relationship(:visiting, ctx.user)
             |> is_empty()

      assert ctx.visitor
             |> Relations.get_bots_by_relationship(:visiting, ctx.user)
             |> has_bot(ctx.bot)

      assert ctx.invitee
             |> Relations.get_bots_by_relationship(:visiting, ctx.user)
             |> is_empty()
    end

    test "subscribed_not_owned", ctx do
      assert ctx.user
             |> Relations.get_bots_by_relationship(
               :subscribed_not_owned,
               ctx.user
             )
             |> is_empty()

      assert ctx.stranger
             |> Relations.get_bots_by_relationship(
               :subscribed_not_owned,
               ctx.user
             )
             |> is_empty()

      assert ctx.subscriber
             |> Relations.get_bots_by_relationship(
               :subscribed_not_owned,
               ctx.user
             )
             |> has_bot(ctx.bot)

      assert ctx.visitor
             |> Relations.get_bots_by_relationship(
               :subscribed_not_owned,
               ctx.user
             )
             |> has_bot(ctx.bot)

      assert ctx.invitee
             |> Relations.get_bots_by_relationship(
               :subscribed_not_owned,
               ctx.user
             )
             |> is_empty()
    end

    test "invited", ctx do
      assert ctx.user
             |> Relations.get_bots_by_relationship(:invited, ctx.user)
             |> is_empty()

      assert ctx.stranger
             |> Relations.get_bots_by_relationship(:invited, ctx.user)
             |> is_empty()

      assert ctx.subscriber
             |> Relations.get_bots_by_relationship(:invited, ctx.user)
             |> is_empty()

      assert ctx.visitor
             |> Relations.get_bots_by_relationship(:invited, ctx.user)
             |> is_empty()

      assert ctx.invitee
             |> Relations.get_bots_by_relationship(:invited, ctx.user)
             |> has_bot(ctx.bot)
    end

    test "people to whom the bot is not visible will not get it as a result",
         ctx do
      assert ctx.user
             |> Relations.get_bots_by_relationship(:visible, ctx.stranger)
             |> is_empty()

      assert ctx.user
             |> Relations.get_bots_by_relationship(:owned, ctx.stranger)
             |> is_empty()

      assert ctx.subscriber
             |> Relations.get_bots_by_relationship(
               :subscribed,
               ctx.stranger
             )
             |> is_empty()

      assert ctx.visitor
             |> Relations.get_bots_by_relationship(:visiting, ctx.stranger)
             |> is_empty()

      assert ctx.subscriber
             |> Relations.get_bots_by_relationship(
               :subscribed_not_owned,
               ctx.stranger
             )
             |> is_empty()

      assert ctx.invitee
             |> Relations.get_bots_by_relationship(:invited, ctx.stranger)
             |> is_empty()
    end
  end

  defp run_is_visible_query(bot, user) do
    Bot
    |> where(id: ^bot.id)
    |> Relations.is_visible_query(user)
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
      Roster.befriend(sub, ctx.user)
      Relations.subscribe(sub, ctx.bot)

      {:ok, sub: sub}
    end

    test "get_subscribers/1", %{bot: bot, user: user} do
      subscribers = Relations.get_subscribers(bot)

      assert length(subscribers) == 1
      assert %User{} = hd(subscribers)
      refute Enum.member?(subscribers, user)
    end

    test "get_subscriber/2", %{bot: bot, sub: sub} do
      assert bot |> Relations.get_subscriber(sub.id) |> same_item(sub)
    end

    test "get_subscriber/2 with non-subscriber", %{bot: bot} do
      refute Relations.get_subscriber(bot, ID.new())
    end
  end

  describe "get_visitors/1" do
    test "should get no visitors when none are present", ctx do
      assert Relations.get_visitors(ctx.bot) == []
    end

    test "should get visitors when they are present", ctx do
      visitor = Factory.insert(:user)
      Roster.befriend(visitor, ctx.user)
      Relations.subscribe(visitor, ctx.bot)
      Relations.visit(visitor, ctx.bot, false)
      assert Relations.get_visitors(ctx.bot) == [visitor]
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

      :ok = Relations.subscribe(friend1, bot)
      :ok = Relations.subscribe(friend2, bot)
      {:error, :permission_denied} = Relations.subscribe(stranger, bot)

      {:ok, friend1: friend1, friend2: friend2, stranger: stranger}
    end

    test "should only include friends", %{
      friend1: friend1,
      friend2: friend2,
      stranger: stranger,
      bot: bot
    } do
      recipients = Relations.notification_recipients(bot, friend1)
      assert Enum.member?(recipients, friend1)
      assert Enum.member?(recipients, friend2)
      refute Enum.member?(recipients, stranger)
    end
  end

  describe "bot relationships" do
    setup ctx do
      other_user = Factory.insert(:user)
      Roster.befriend(ctx.user, other_user)

      owned_bot = Factory.insert(:bot, user: ctx.user)
      invited_bot = Factory.insert(:bot, user: other_user)
      subscribed_bot = Factory.insert(:bot, user: other_user)
      unaffiliated_bot = Factory.insert(:bot, user: other_user)

      Relations.invite(ctx.user, invited_bot, other_user)
      Relations.subscribe(ctx.user, subscribed_bot)

      {:ok,
       other_user: other_user,
       owned_bot: owned_bot,
       invited_bot: invited_bot,
       subscribed_bot: subscribed_bot,
       unaffiliated_bot: unaffiliated_bot}
    end

    test "owned?/2", ctx do
      assert Relations.owned?(ctx.user, ctx.owned_bot)
      refute Relations.owned?(ctx.user, ctx.invited_bot)
      refute Relations.owned?(ctx.user, ctx.subscribed_bot)
      refute Relations.owned?(ctx.user, ctx.unaffiliated_bot)
    end

    test "invited?/2", ctx do
      refute Relations.invited?(ctx.user, ctx.owned_bot)
      assert Relations.invited?(ctx.user, ctx.invited_bot)
      refute Relations.invited?(ctx.user, ctx.subscribed_bot)
      refute Relations.invited?(ctx.user, ctx.unaffiliated_bot)
    end

    test "subscribed?/2", ctx do
      refute Relations.subscribed?(ctx.user, ctx.invited_bot)
      assert Relations.subscribed?(ctx.user, ctx.subscribed_bot)
      refute Relations.subscribed?(ctx.user, ctx.unaffiliated_bot)
    end

    test "visiting?/2", ctx do
      refute Relations.visiting?(ctx.user, ctx.owned_bot)
      refute Relations.visiting?(ctx.user, ctx.invited_bot)
      refute Relations.visiting?(ctx.user, ctx.subscribed_bot)
      refute Relations.visiting?(ctx.user, ctx.unaffiliated_bot)

      Relations.visit(ctx.user, ctx.subscribed_bot)
      assert Relations.visiting?(ctx.user, ctx.subscribed_bot)
    end

    test "visible?/2", ctx do
      assert Relations.visible?(ctx.user, ctx.owned_bot)
      assert Relations.visible?(ctx.user, ctx.invited_bot)
      assert Relations.visible?(ctx.user, ctx.subscribed_bot)
      refute Relations.visible?(ctx.user, ctx.unaffiliated_bot)
    end

    test "get_bot_relationships/2", ctx do
      rels! = Relations.get_bot_relationships(ctx.user, ctx.owned_bot)

      assert length(rels!) == 2
      assert Enum.member?(rels!, :visible)
      assert Enum.member?(rels!, :owned)

      rels! = Relations.get_bot_relationships(ctx.user, ctx.invited_bot)

      assert length(rels!) == 2
      assert Enum.member?(rels!, :visible)
      assert Enum.member?(rels!, :invited)

      rels! = Relations.get_bot_relationships(ctx.user, ctx.subscribed_bot)

      assert length(rels!) == 2
      assert Enum.member?(rels!, :visible)
      assert Enum.member?(rels!, :subscribed)
    end
  end

  # -------------------------------------------------------------------
  # Subsriptions

  def create_subscription(ctx) do
    [user, visitor] = Factory.insert_list(2, :user)
    Roster.befriend(ctx.user, user)
    Roster.befriend(ctx.user, visitor)

    Factory.insert(:subscription, user: user, bot: ctx.bot)

    Factory.insert(
      :subscription,
      user: visitor,
      bot: ctx.bot,
      visitor: true
    )

    {:ok, owner: ctx.user, user: user, visitor: visitor}
  end

  describe "get_subscription/2" do
    setup :create_subscription

    test "should return the subscription", ctx do
      assert Relations.get_subscription(ctx.user, ctx.bot)
    end

    test "should return nil when the user does not exist", ctx do
      user = Factory.build(:user)
      refute Relations.get_subscription(user, ctx.bot)
    end

    test "should return nil when the bot does not exist", ctx do
      bot = Factory.build(:bot)
      refute Relations.get_subscription(ctx.user, bot)
    end

    test "should return nil when the user is not subscribed to the bot", ctx do
      refute Relations.get_subscription(ctx.owner, ctx.bot)
    end
  end

  describe "subscribe/2" do
    setup :create_subscription

    test "when a subscription does not already exist", ctx do
      new_user = Factory.insert(:user)
      Roster.befriend(new_user, ctx.owner)

      result = Relations.subscribe(new_user, ctx.bot)

      assert result == :ok
      assert Relations.subscribed?(new_user, ctx.bot)
    end

    test "when a subscription already exists", ctx do
      result = Relations.subscribe(ctx.visitor, ctx.bot)

      assert result == :ok
      assert Relations.visiting?(ctx.visitor, ctx.bot)
    end
  end

  describe "unsubscribe/2" do
    setup :create_subscription

    test "when a subscription exists", ctx do
      result = Relations.unsubscribe(ctx.user, ctx.bot)

      assert result == :ok
      refute Relations.subscribed?(ctx.user, ctx.bot)
    end

    test "when the bot owner is trying to unsubscribe", ctx do
      assert {:error, _} = Relations.unsubscribe(ctx.owner, ctx.bot)
    end

    test "when the user doesn't exist", ctx do
      user = Factory.build(:user)

      assert Relations.unsubscribe(user, ctx.bot) == :ok
    end

    test "when the bot doesn't exist", ctx do
      bot = Factory.build(:bot)

      assert Relations.unsubscribe(ctx.user, bot) == :ok
    end
  end

  describe "visit/2" do
    setup :create_subscription

    test "should set the subscriber as a visitor", ctx do
      assert Relations.visit(ctx.user, ctx.bot) == :ok
      assert Relations.visiting?(ctx.user, ctx.bot)
    end
  end

  describe "depart/2" do
    setup :create_subscription

    test "should set the visitor as a subscriber", ctx do
      assert Relations.depart(ctx.visitor, ctx.bot) == :ok
      refute Relations.visiting?(ctx.visitor, ctx.bot)
    end
  end

  describe "delete_subscriptions_for_owned_bots/2" do
    setup :create_subscription

    test "should delete subscriptions to bots owned by the specified user",
         ctx do
      assert :ok == Relations.delete_subscriptions_for_owned_bots(ctx.owner, ctx.user)
      refute Relations.subscribed?(ctx.user, ctx.bot)
    end

    test "should not delete subscriptions to bots owned by another user", ctx do
      u = Factory.insert(:user)
      assert :ok == Relations.delete_subscriptions_for_owned_bots(u, ctx.user)
      assert Relations.subscribed?(ctx.user, ctx.bot)
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
      refute is_subscribed_sp(ctx.owner, ctx.bot)
    end
  end

  # -------------------------------------------------------------------
  # Invitations

  describe "invite/3" do
    setup ctx do
      [invitee, stranger] = Factory.insert_list(2, :user, device: "testing")
      Roster.befriend(ctx.user, invitee)

      {:ok, invitee: invitee, stranger: stranger}
    end

    test "should create an invitation", ctx do
      assert {:ok, invitation} = Relations.invite(ctx.invitee, ctx.bot, ctx.user)

      assert invitation == Repo.get_by(Invitation, id: invitation.id)
      refute invitation.accepted
    end

    test "refuse invitation to non-owned bot", ctx do
      other_user = Factory.insert(:user)

      assert {:error, :permission_denied} ==
               Relations.invite(ctx.invitee, ctx.bot, other_user)
    end

    test "refuse invitation to non-friend", ctx do
      assert {:error, :permission_denied} =
               Relations.invite(ctx.stranger, ctx.bot, ctx.user)
    end

    test "subsequent invitations should overwrite existing ones", ctx do
      assert {:ok, invitation} = Relations.invite(ctx.invitee, ctx.bot, ctx.user)

      assert {:ok, invitation2} = Relations.invite(ctx.invitee, ctx.bot, ctx.user)

      assert invitation.id == invitation2.id

      assert DateTime.compare(invitation.updated_at, invitation2.updated_at) ==
               :lt
    end
  end

  defp setup_invitation(ctx) do
    invitee = Factory.insert(:user, device: "testing")
    Roster.befriend(ctx.user, invitee)

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
      assert %Invitation{} = Relations.get_invitation(ctx.invitation.id, ctx.invitee)
    end

    test "Unrelated user cannot get the invitation", ctx do
      refute Relations.get_invitation(ctx.invitation.id, Factory.insert(:user))
    end
  end

  describe "respond/3" do
    setup :setup_invitation

    test "Invitee can accept", ctx do
      assert {:ok, invitation} =
               Relations.respond(ctx.invitation, true, ctx.invitee)

      assert invitation.accepted == true
    end

    test "Invitee becomes subscribed if they accept", ctx do
      refute Relations.subscribed?(ctx.invitee, ctx.bot)

      assert {:ok, invitation} =
               Relations.respond(ctx.invitation, true, ctx.invitee)

      assert Relations.subscribed?(ctx.invitee, ctx.bot)
    end

    test "Invitee can decline", ctx do
      assert {:ok, invitation} =
               Relations.respond(ctx.invitation, false, ctx.invitee)

      assert invitation.accepted == false
    end

    test "Invitee does not become subscribed if they decline", ctx do
      refute Relations.subscribed?(ctx.invitee, ctx.bot)

      assert {:ok, invitation} =
               Relations.respond(ctx.invitation, false, ctx.invitee)

      refute Relations.subscribed?(ctx.invitee, ctx.bot)
    end

    test "Inviter cannot respond", ctx do
      assert {:error, :permission_denied} =
               Relations.respond(ctx.invitation, false, ctx.user)
    end

    test "Other user cannot respond", ctx do
      assert {:error, :permission_denied} =
               Relations.respond(
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

      Relations.delete_invitation(user, invitee)

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
