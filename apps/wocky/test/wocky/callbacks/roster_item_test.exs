defmodule Wocky.Callbacks.RosterItemTest do
  use Wocky.WatcherCase

  alias Wocky.Bots.Invitation
  alias Wocky.Callbacks.RosterItem, as: Callback
  alias Wocky.Location
  alias Wocky.Relations
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
      Relations.subscribe(ctx.user_bot, ctx.contact)
      Relations.subscribe(ctx.contact_bot, ctx.user)

      Roster.unfriend(ctx.user, ctx.contact)

      refute_eventually(Relations.subscription(ctx.user_bot, ctx.contact))
      refute_eventually(Relations.subscription(ctx.contact_bot, ctx.user))
    end

    test "bot invitations should be removed", ctx do
      Invitation.put(ctx.contact, ctx.user_bot, ctx.user)
      Invitation.put(ctx.user, ctx.contact_bot, ctx.contact)

      Roster.unfriend(ctx.user, ctx.contact)

      refute_eventually(Invitation.get(ctx.user_bot, ctx.contact))
      refute_eventually(Invitation.get(ctx.contact_bot, ctx.user))
    end

    test "locations shares should be canceled", ctx do
      expiry = Timestamp.shift(days: 1)
      Location.start_sharing_location(ctx.user, ctx.contact, expiry)

      Roster.unfriend(ctx.user, ctx.contact)

      assert_eventually(Location.get_location_shares(ctx.user) == [])
    end
  end
end
