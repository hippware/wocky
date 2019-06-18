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

  setup do
    user = Factory.insert(:user)
    bot = Factory.insert(:bot, user: user)

    {:ok,
     user: user,
     bot: bot,
     id: user.id,
     external_id: user.external_id,
     phone_number: user.phone_number}
  end

  describe "get_bot/3" do
    setup ctx do
      pending = Factory.insert(:bot, user: ctx.user, pending: true)

      {:ok, pending: pending}
    end

    test "should return the requested bot for the owner",
         %{bot: bot, user: user} do
      assert bot.id |> Relations.get_bot(user) |> Repo.preload(:user) == bot
    end

    test "should return no bot for a stranger",
         %{bot: bot} do
      stranger = Factory.insert(:user)
      assert bot.id |> Relations.get_bot(stranger) == nil
    end

    test "should return the bot for a subscriber", ctx do
      subscriber = Factory.insert(:user)
      Roster.befriend(subscriber, ctx.user)
      Relations.subscribe(subscriber, ctx.bot)

      assert ctx.bot.id |> Relations.get_bot(subscriber) |> Repo.preload(:user) ==
               ctx.bot
    end

    test "should not return pending bot by default",
         %{pending: pending, user: user} do
      assert pending.id |> Relations.get_bot(user) == nil
    end

    test "should return pending bot if explicity requested",
         %{pending: pending, user: user} do
      assert pending.id |> Relations.get_bot(user, true) |> Repo.preload(:user) ==
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
      assert bot.id |> Relations.get_owned_bot(user) |> Repo.preload(:user) == bot
    end

    test "should return no bot for a stranger",
         %{bot: bot} do
      stranger = Factory.insert(:user)
      assert bot.id |> Relations.get_owned_bot(stranger) == nil
    end

    test "should return no bot for a subscriber", %{bot: bot} do
      subscriber = Factory.insert(:user)
      Relations.subscribe(subscriber, bot)
      assert bot.id |> Relations.get_owned_bot(subscriber) == nil
    end

    test "should not return pending bot by default",
         %{pending: pending, user: user} do
      assert pending.id |> Relations.get_owned_bot(user) == nil
    end

    test "should return pending bot if explicity requested",
         %{pending: pending, user: user} do
      assert pending.id |> Relations.get_owned_bot(user, true) |> Repo.preload(:user) ==
               pending
    end
  end

  describe "active_bots_query/1" do
    setup ctx do
      subscriber = Factory.insert(:user)
      Roster.befriend(subscriber, ctx.user)
      Relations.subscribe(subscriber, ctx.bot)

      {:ok, subscriber: subscriber}
    end

    test "should not return unvisted bot", ctx do
      assert ctx.subscriber |> Relations.active_bots_query() |> is_empty()
    end

    test "should return visted bot", ctx do
      Relations.visit(ctx.subscriber, ctx.bot, false)
      assert ctx.subscriber |> Relations.active_bots_query() |> has_bot(ctx.bot)
    end

    test "should return bots with most recently visited first", ctx do
      bot2 = Factory.insert(:bot, user: ctx.user)
      Relations.subscribe(ctx.subscriber, bot2)
      Relations.visit(ctx.subscriber, ctx.bot, false)
      Relations.visit(ctx.subscriber, bot2, false)

      bot_ids =
        ctx.subscriber
        |> Relations.active_bots_query()
        |> Repo.all()
        |> Enum.map(& &1.id)

      assert bot_ids == [bot2.id, ctx.bot.id]
    end
  end

  describe "filter_by_location/3" do
    test "finds included bot", ctx do
      a = GeoUtils.point(Bots.lat(ctx.bot) - 0.1, Bots.lon(ctx.bot) - 0.1)
      b = GeoUtils.point(Bots.lat(ctx.bot) + 0.1, Bots.lon(ctx.bot) + 0.1)

      assert Bot
             |> where(id: ^ctx.bot.id)
             |> Relations.filter_by_location(a, b)
             |> Repo.one()
             |> Repo.preload(:user) == ctx.bot
    end

    test "does not find excluded bot", ctx do
      a = GeoUtils.point(Bots.lat(ctx.bot) - 0.1, Bots.lon(ctx.bot) - 0.1)
      b = GeoUtils.point(Bots.lat(ctx.bot) - 0.2, Bots.lon(ctx.bot) + 0.1)

      assert Bot
             |> where(id: ^ctx.bot.id)
             |> Relations.filter_by_location(a, b)
             |> Repo.one() == nil
    end
  end

  describe "bot relationships" do
    setup ctx do
      other_user = Factory.insert(:user)
      Roster.befriend(ctx.user, other_user)

      owned_bot = Factory.insert(:bot, user: ctx.user)
      pending_bot = Factory.insert(:bot, user: ctx.user, pending: true)
      invited_bot = Factory.insert(:bot, user: other_user)
      subscribed_bot = Factory.insert(:bot, user: other_user)
      unaffiliated_bot = Factory.insert(:bot, user: other_user)

      Relations.invite(ctx.user, invited_bot, other_user)
      Relations.subscribe(ctx.user, subscribed_bot)

      {:ok,
       other_user: other_user,
       owned_bot: owned_bot,
       pending_bot: pending_bot,
       invited_bot: invited_bot,
       subscribed_bot: subscribed_bot,
       unaffiliated_bot: unaffiliated_bot}
    end

    test "can_access?/2", ctx do
      assert Relations.can_access?(ctx.user, ctx.owned_bot)
      assert Relations.can_access?(ctx.user, ctx.invited_bot)
      refute Relations.can_access?(ctx.user, ctx.unaffiliated_bot)
    end

    test "get_subscriptions/1", ctx do
      subscriptions = Relations.get_subscriptions(ctx.user)

      assert length(subscriptions) == 1
      assert Enum.any?(subscriptions, &same_bot(&1, ctx.subscribed_bot))
      refute Enum.any?(subscriptions, &same_bot(&1, ctx.owned_bot))
      refute Enum.any?(subscriptions, &same_bot(&1, ctx.pending_bot))
    end

    test "get_owned_bots/1", ctx do
      bots = Relations.get_owned_bots(ctx.user)

      assert length(bots) == 2
      assert Enum.any?(bots, &same_bot(&1, ctx.owned_bot))
      refute Enum.any?(bots, &same_bot(&1, ctx.pending_bot))
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

  describe "by_relationship_query/3" do
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
             |> Relations.by_relationship_query(:visible, ctx.user)
             |> has_bot(ctx.bot)

      assert ctx.stranger
             |> Relations.by_relationship_query(:visible, ctx.user)
             |> is_empty()

      assert ctx.subscriber
             |> Relations.by_relationship_query(:visible, ctx.user)
             |> has_bot(ctx.bot)

      assert ctx.visitor
             |> Relations.by_relationship_query(:visible, ctx.user)
             |> has_bot(ctx.bot)

      assert ctx.invitee
             |> Relations.by_relationship_query(:visible, ctx.user)
             |> has_bot(ctx.bot)
    end

    test "subscribed", ctx do
      assert ctx.user
             |> Relations.by_relationship_query(:subscribed, ctx.user)
             |> has_bot(ctx.bot)

      assert ctx.stranger
             |> Relations.by_relationship_query(:subscribed, ctx.user)
             |> is_empty()

      assert ctx.subscriber
             |> Relations.by_relationship_query(:subscribed, ctx.user)
             |> has_bot(ctx.bot)

      assert ctx.visitor
             |> Relations.by_relationship_query(:subscribed, ctx.user)
             |> has_bot(ctx.bot)

      assert ctx.invitee
             |> Relations.by_relationship_query(:subscribed, ctx.user)
             |> is_empty()
    end

    test "owned", ctx do
      assert ctx.user
             |> Relations.by_relationship_query(:owned, ctx.user)
             |> has_bot(ctx.bot)

      assert ctx.stranger
             |> Relations.by_relationship_query(:owned, ctx.user)
             |> is_empty()

      assert ctx.subscriber
             |> Relations.by_relationship_query(:owned, ctx.user)
             |> is_empty()

      assert ctx.visitor
             |> Relations.by_relationship_query(:owned, ctx.user)
             |> is_empty()

      assert ctx.invitee
             |> Relations.by_relationship_query(:owned, ctx.user)
             |> is_empty()
    end

    test "visiting", ctx do
      assert ctx.user
             |> Relations.by_relationship_query(:visiting, ctx.user)
             |> is_empty()

      assert ctx.stranger
             |> Relations.by_relationship_query(:visiting, ctx.user)
             |> is_empty()

      assert ctx.subscriber
             |> Relations.by_relationship_query(:visiting, ctx.user)
             |> is_empty()

      assert ctx.visitor
             |> Relations.by_relationship_query(:visiting, ctx.user)
             |> has_bot(ctx.bot)

      assert ctx.invitee
             |> Relations.by_relationship_query(:visiting, ctx.user)
             |> is_empty()
    end

    test "subscribed_not_owned", ctx do
      assert ctx.user
             |> Relations.by_relationship_query(
               :subscribed_not_owned,
               ctx.user
             )
             |> is_empty()

      assert ctx.stranger
             |> Relations.by_relationship_query(
               :subscribed_not_owned,
               ctx.user
             )
             |> is_empty()

      assert ctx.subscriber
             |> Relations.by_relationship_query(
               :subscribed_not_owned,
               ctx.user
             )
             |> has_bot(ctx.bot)

      assert ctx.visitor
             |> Relations.by_relationship_query(
               :subscribed_not_owned,
               ctx.user
             )
             |> has_bot(ctx.bot)

      assert ctx.invitee
             |> Relations.by_relationship_query(
               :subscribed_not_owned,
               ctx.user
             )
             |> is_empty()
    end

    test "invited", ctx do
      assert ctx.user
             |> Relations.by_relationship_query(:invited, ctx.user)
             |> is_empty()

      assert ctx.stranger
             |> Relations.by_relationship_query(:invited, ctx.user)
             |> is_empty()

      assert ctx.subscriber
             |> Relations.by_relationship_query(:invited, ctx.user)
             |> is_empty()

      assert ctx.visitor
             |> Relations.by_relationship_query(:invited, ctx.user)
             |> is_empty()

      assert ctx.invitee
             |> Relations.by_relationship_query(:invited, ctx.user)
             |> has_bot(ctx.bot)
    end

    test "people to whom the bot is not visible will not get it as a result",
         ctx do
      assert ctx.user
             |> Relations.by_relationship_query(:visible, ctx.stranger)
             |> is_empty()

      assert ctx.user
             |> Relations.by_relationship_query(:owned, ctx.stranger)
             |> is_empty()

      assert ctx.subscriber
             |> Relations.by_relationship_query(
               :subscribed,
               ctx.stranger
             )
             |> is_empty()

      assert ctx.visitor
             |> Relations.by_relationship_query(:visiting, ctx.stranger)
             |> is_empty()

      assert ctx.subscriber
             |> Relations.by_relationship_query(
               :subscribed_not_owned,
               ctx.stranger
             )
             |> is_empty()

      assert ctx.invitee
             |> Relations.by_relationship_query(:invited, ctx.stranger)
             |> is_empty()
    end
  end

  describe "subscribers" do
    setup ctx do
      sub = Factory.insert(:user)
      Roster.befriend(sub, ctx.user)
      Relations.subscribe(sub, ctx.bot)

      {:ok, sub: sub}
    end

    test "subscribers_query/1", %{bot: bot, user: user} do
      subscribers = bot |> Relations.subscribers_query() |> Repo.all()

      assert length(subscribers) == 1
      assert %User{} = hd(subscribers)
      refute Enum.member?(subscribers, user)
    end

    test "subscriber_query/2", %{bot: bot, sub: sub} do
      subscriber = bot |> Relations.subscriber_query(sub.id) |> Repo.one()

      assert subscriber.id == sub.id
    end

    test "subscriber_query/2 with non-subscriber", %{bot: bot} do
      assert bot |> Relations.subscriber_query(ID.new()) |> Repo.one() == nil
    end
  end

  describe "visitors_query/1" do
    test "should get no visitors when none are present", ctx do
      assert ctx.bot |> Relations.visitors_query() |> Repo.one() == nil
    end

    test "should get visitors when they are present", ctx do
      visitor = Factory.insert(:user)
      Roster.befriend(visitor, ctx.user)
      Relations.subscribe(visitor, ctx.bot)
      Relations.visit(visitor, ctx.bot, false)
      assert ctx.bot |> Relations.visitors_query() |> Repo.one() == visitor
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

  defp run_is_visible_query(bot, user) do
    Bot
    |> where(id: ^bot.id)
    |> Relations.is_visible_query(user)
    |> preload(:user)
    |> Repo.one()
  end

  defp has_bot(query, bot),
    do: query |> Repo.one() |> Repo.preload(:user) == bot

  defp is_empty(query),
    do: query |> Repo.one() == nil

  defp same_bot(bot1, bot2), do: bot1.id == bot2.id

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

  describe "subscription/2" do
    setup :create_subscription

    test "should return :subscribed if the user is subscribed to the bot",
         ctx do
      assert Relations.subscription(ctx.user, ctx.bot) == :subscribed
    end

    test "should return nil when the user does not exist", ctx do
      user = Factory.build(:user, device: "testing")
      refute Relations.subscription(user, ctx.bot)
    end

    test "should return nil when the bot does not exist", ctx do
      bot = Factory.build(:bot)
      refute Relations.subscription(ctx.user, bot)
    end

    test "should return nil when the user is not subscribed to the bot", ctx do
      refute Relations.subscription(ctx.owner, ctx.bot)
    end

    test "should return :visitor when the user is a visitor", ctx do
      assert Relations.subscription(ctx.visitor, ctx.bot) == :visiting
    end
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
      assert Relations.subscription(new_user, ctx.bot) == :subscribed
    end

    test "when a subscription already exists", ctx do
      result = Relations.subscribe(ctx.visitor, ctx.bot)

      assert result == :ok
      assert Relations.subscription(ctx.visitor, ctx.bot) == :visiting
    end
  end

  describe "unsubscribe/2" do
    setup :create_subscription

    test "when a subscription exists", ctx do
      result = Relations.unsubscribe(ctx.user, ctx.bot)

      assert result == :ok
      refute Relations.subscription(ctx.user, ctx.bot)
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

  describe "delete_subscriptions_for_owned_bots/2" do
    setup :create_subscription

    test "should delete subscriptions to bots owned by the specified user",
         ctx do
      assert :ok == Relations.delete_subscriptions_for_owned_bots(ctx.owner, ctx.user)
      refute Relations.subscription(ctx.user, ctx.bot)
    end

    test "should not delete subscriptions to bots owned by another user", ctx do
      u = Factory.insert(:user)
      assert :ok == Relations.delete_subscriptions_for_owned_bots(u, ctx.user)
      assert Relations.subscription(ctx.user, ctx.bot) == :subscribed
    end
  end

  describe "visit/2" do
    setup :create_subscription

    test "should set the subscriber as a visitor", ctx do
      assert Relations.visit(ctx.user, ctx.bot) == :ok
      assert Relations.subscription(ctx.user, ctx.bot) == :visiting
    end
  end

  describe "depart/2" do
    setup :create_subscription

    test "should set the visitor as a subscriber", ctx do
      assert Relations.depart(ctx.visitor, ctx.bot) == :ok
      assert Relations.subscription(ctx.visitor, ctx.bot) == :subscribed
    end
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

  defp is_subscribed_sp(user, bot) do
    {:ok, u} = Ecto.UUID.dump(user.id)
    {:ok, b} = Ecto.UUID.dump(bot.id)

    Repo
    |> SQL.query!("SELECT is_subscribed($1, $2)", [u, b])
    |> Map.get(:rows)
    |> hd
    |> hd
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
      assert invitation.accepted == nil
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

    test "User can get their own invitation", ctx do
      assert %Invitation{} = Relations.get_invitation(ctx.invitation.id, ctx.user)
    end

    test "Invitee can get their own invitation", ctx do
      assert %Invitation{} = Relations.get_invitation(ctx.invitation.id, ctx.invitee)
    end

    test "Unrelated user cannot get the invitation", ctx do
      refute Relations.get_invitation(ctx.invitation.id, Factory.insert(:user))
    end
  end

  describe "get_invitation/2 by bot id" do
    setup :setup_invitation

    test "User can get their own invitation", ctx do
      assert %Invitation{} = Relations.get_invitation(ctx.bot, ctx.user)
    end

    test "Invitee can get their own invitation", ctx do
      assert %Invitation{} = Relations.get_invitation(ctx.bot, ctx.invitee)
    end

    test "Unrelated user cannot get the invitation", ctx do
      assert nil == Relations.get_invitation(ctx.bot, Factory.insert(:user))
    end
  end

  describe "exists?/2 - true" do
    setup :setup_invitation

    test "exists by id", ctx do
      assert Relations.exists?(ctx.invitation.id, ctx.user)
    end

    test "exists by bot", ctx do
      assert Relations.exists?(ctx.bot, ctx.user)
    end
  end

  describe "exists?/2 - false" do
    test "does not exist by id", ctx do
      refute Relations.exists?(1, ctx.user)
    end

    test "does not exist by bot", ctx do
      refute Relations.exists?(ctx.bot, ctx.user)
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
      assert Relations.subscription(ctx.invitee, ctx.bot) == nil

      assert {:ok, invitation} =
               Relations.respond(ctx.invitation, true, ctx.invitee)

      assert Relations.subscription(ctx.invitee, ctx.bot) == :subscribed
    end

    test "Invitee can decline", ctx do
      assert {:ok, invitation} =
               Relations.respond(ctx.invitation, false, ctx.invitee)

      assert invitation.accepted == false
    end

    test "Invitee does not become subscribed if they decline", ctx do
      assert Relations.subscription(ctx.invitee, ctx.bot) == nil

      assert {:ok, invitation} =
               Relations.respond(ctx.invitation, false, ctx.invitee)

      assert Relations.subscription(ctx.invitee, ctx.bot) == nil
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
