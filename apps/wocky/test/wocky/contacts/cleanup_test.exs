defmodule Wocky.Contacts.CleanupTest do
  use Wocky.DataCase

  # Tests the database cleanup done by triggers in the database.

  import Eventually

  alias Wocky.Contacts
  alias Wocky.Notifier.InBand.Notification
  alias Wocky.POI
  alias Wocky.Relation
  alias Wocky.Repo.ID

  setup do
    [user, contact] = Factory.insert_list(2, :user)

    :ok = Contacts.befriend(user, contact, :always)

    user_bot = Factory.insert(:bot, user: user)
    contact_bot = Factory.insert(:bot, user: contact)

    Relation.subscribe(user, contact_bot)
    Relation.subscribe(contact, user_bot)

    Relation.invite(user, contact_bot, contact)
    Relation.invite(contact, user_bot, user)

    {:ok,
     user: user, contact: contact, user_bot: user_bot, contact_bot: contact_bot}
  end

  describe "post-unfriend cleanup" do
    setup ctx do
      :ok = Contacts.unfriend(ctx.user, ctx.contact)
    end

    test "should delete invitations to the other user's bots", ctx do
      refute_eventually(Relation.invited?(ctx.user, ctx.contact_bot))
      refute_eventually(Relation.invited?(ctx.contact, ctx.user_bot))
    end

    test "should unsubscribe from the other user's bots", ctx do
      refute_eventually(Relation.subscribed?(ctx.user, ctx.contact_bot))
      refute_eventually(Relation.subscribed?(ctx.contact, ctx.user_bot))
    end
  end

  describe "post-block cleanup" do
    setup ctx do
      notification1 =
        Factory.insert(:bot_invitation_notification,
          user: ctx.user,
          other_user: ctx.contact
        )

      notification2 =
        Factory.insert(:bot_invitation_notification,
          user: ctx.contact,
          other_user: ctx.user
        )

      {:ok, contact_item} =
        POI.put_item(ctx.user_bot, ID.new(), "testing", nil, ctx.contact)

      {:ok, user_item} =
        POI.put_item(ctx.contact_bot, ID.new(), "testing", nil, ctx.user)

      :ok = Contacts.block(ctx.user, ctx.contact)

      {:ok,
       notification1: notification1,
       notification2: notification2,
       contact_item: contact_item,
       user_item: user_item}
    end

    test "should delete invitations to the other user's bots", ctx do
      refute_eventually(Relation.invited?(ctx.user, ctx.contact_bot))
      refute_eventually(Relation.invited?(ctx.contact, ctx.user_bot))
    end

    test "should unsubscribe from the other user's bots", ctx do
      refute_eventually(Relation.subscribed?(ctx.user, ctx.contact_bot))
      refute_eventually(Relation.subscribed?(ctx.contact, ctx.user_bot))
    end

    test "should delete bot items", ctx do
      refute_eventually(POI.get_item(ctx.user_bot, ctx.contact_item.id))
      refute_eventually(POI.get_item(ctx.contact_bot, ctx.user_item.id))
    end

    test "should delete notifications between the two users", ctx do
      refute_eventually(Repo.get(Notification, ctx.notification1.id))
      refute_eventually(Repo.get(Notification, ctx.notification2.id))
    end
  end
end
