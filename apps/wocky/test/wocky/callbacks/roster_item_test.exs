defmodule Wocky.Callbacks.RosterItemTest do
  use Wocky.WatcherCase

  alias Wocky.Callbacks.RosterItem, as: Callback
  alias Wocky.Location
  alias Wocky.Relation
  alias Wocky.Repo.Factory
  alias Wocky.Repo.Timestamp
  alias Wocky.Roster

  setup_all do
    Callback.register()
  end

  setup do
    user = Factory.insert(:user)
    contact = Factory.insert(:user)

    Roster.befriend(user, contact)

    user_bot = Factory.insert(:bot, user: user)
    contact_bot = Factory.insert(:bot, user: contact)

    {:ok,
     user: user, contact: contact, user_bot: user_bot, contact_bot: contact_bot}
  end

  describe "unfriend cleanup" do
    test "bots should no longer be subscribed", ctx do
      Relation.subscribe(ctx.contact, ctx.user_bot)
      Relation.subscribe(ctx.user, ctx.contact_bot)

      Roster.unfriend(ctx.user, ctx.contact)

      refute_eventually(Relation.subscribed?(ctx.contact, ctx.user_bot))
      refute_eventually(Relation.subscribed?(ctx.user, ctx.contact_bot))
    end

    test "bot invitations should be removed", ctx do
      Relation.invite(ctx.contact, ctx.user_bot, ctx.user)
      Relation.invite(ctx.user, ctx.contact_bot, ctx.contact)

      Roster.unfriend(ctx.user, ctx.contact)

      refute_eventually(Relation.invited?(ctx.contact, ctx.user_bot))
      refute_eventually(Relation.invited?(ctx.user, ctx.contact_bot))
    end

    test "locations shares should be canceled", ctx do
      expiry = Timestamp.shift(days: 1)
      Location.start_sharing_location(ctx.user, ctx.contact, expiry)

      Roster.unfriend(ctx.user, ctx.contact)

      assert_eventually(Location.get_location_shares(ctx.user) == [])
    end
  end
end
