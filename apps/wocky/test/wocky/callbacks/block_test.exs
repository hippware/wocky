defmodule Wocky.Callbacks.BlockTest do
  use Wocky.WatcherCase

  alias Wocky.Block
  alias Wocky.Callbacks.Block, as: Callback
  alias Wocky.Notifier.InBand.Notification
  alias Wocky.Relations.Invitation
  alias Wocky.Repo.Factory

  setup_all do
    Callback.register()
  end

  setup do
    [u1, u2] = Factory.insert_list(2, :user)

    {:ok, user1: u1, user2: u2}
  end

  describe "block-triggered item deletion" do
    setup %{user1: user1, user2: user2} do
      invitation1 =
        Factory.insert(:user_invitation, user: user1, invitee: user2)

      invitation2 =
        Factory.insert(:user_invitation, user: user2, invitee: user1)

      notification1 =
        Factory.insert(:bot_invitation_notification,
          user: user1,
          other_user: user2
        )

      notification2 =
        Factory.insert(:bot_invitation_notification,
          user: user2,
          other_user: user1
        )

      Block.block(user1, user2)

      {:ok,
       invitation1: invitation1,
       invitation2: invitation2,
       notification1: notification1,
       notification2: notification2}
    end

    test "should delete invitations between the two users", ctx do
      refute_eventually(Repo.get(Invitation, ctx.invitation1.id), 500, 10)
      refute_eventually(Repo.get(Invitation, ctx.invitation2.id))
    end

    test "should delete notifications between the two users", ctx do
      refute_eventually(Repo.get(Notification, ctx.notification1.id))
      refute_eventually(Repo.get(Notification, ctx.notification2.id))
    end
  end
end
