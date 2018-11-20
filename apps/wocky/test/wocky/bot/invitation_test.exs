defmodule Wocky.Bot.InvitationTest do
  use Wocky.WatcherHelper, async: false

  alias Faker.Code
  alias Pigeon.APNS.Notification
  alias Wocky.Bot
  alias Wocky.Bot.Invitation
  alias Wocky.Push
  alias Wocky.Push.Sandbox
  alias Wocky.Repo
  alias Wocky.Repo.Factory

  setup do
    [user, invitee] = Factory.insert_list(2, :user, device: "testing")
    bot = Factory.insert(:bot, user: user)

    Sandbox.clear_notifications(global: true)

    :ok = Push.enable(invitee, invitee.device, Code.isbn13())
    :ok = Push.enable(user, user.device, Code.isbn13())

    {:ok, user: user, invitee: invitee, bot: bot}
  end

  describe "put/3" do
    test "should create an invitation", ctx do
      assert {:ok, invitation} = Invitation.put(ctx.invitee, ctx.bot, ctx.user)

      assert invitation == Repo.get_by(Invitation, id: invitation.id)
      assert invitation.accepted == nil

      msgs = Sandbox.wait_notifications(count: 1, timeout: 500, global: true)
      assert length(msgs) == 1

      assert %Notification{
               payload: %{
                 "aps" => %{"alert" => message}
               }
             } = hd(msgs)

      assert message ==
               "@#{ctx.user.handle} invited you to follow #{ctx.bot.title}"

      clear_expected_notifications(1)
    end

    test "refuse invitation to non-owned bot", ctx do
      other_user = Factory.insert(:user)

      assert {:error, :permission_denied} ==
               Invitation.put(ctx.invitee, ctx.bot, other_user)

      assert no_more_push_notifications()
    end

    test "subsequent invitations should overwrite existing ones", ctx do
      assert {:ok, invitation} = Invitation.put(ctx.invitee, ctx.bot, ctx.user)

      assert {:ok, invitation2} = Invitation.put(ctx.invitee, ctx.bot, ctx.user)

      assert invitation.id == invitation2.id

      assert DateTime.compare(invitation.updated_at, invitation2.updated_at) ==
               :lt

      assert clear_expected_notifications(1)
    end
  end

  describe "get/3" do
    setup ctx do
      %{id: id} =
        Factory.insert(:invitation,
          user: ctx.user,
          invitee: ctx.invitee
        )

      clear_expected_notifications(1)

      {:ok, id: id}
    end

    test "User can get their own invitation", ctx do
      assert %Invitation{} = Invitation.get(ctx.id, ctx.user)
    end

    test "Invitee can get their own invitation", ctx do
      assert %Invitation{} = Invitation.get(ctx.id, ctx.invitee)
    end

    test "Unrelated user cannot get the invitation", ctx do
      assert nil == Invitation.get(ctx.id, Factory.insert(:user))
    end
  end

  describe "respond/3" do
    setup ctx do
      invitation =
        Factory.insert(:invitation,
          user: ctx.user,
          invitee: ctx.invitee,
          bot: ctx.bot
        )

      clear_expected_notifications(1)

      {:ok, invitation: invitation}
    end

    test "Invitee can accept", ctx do
      assert {:ok, invitation} =
               Invitation.respond(ctx.invitation, true, ctx.invitee)

      assert invitation.accepted == true

      clear_expected_notifications(1)
    end

    test "Invitee becomes subscribed if they accept", ctx do
      assert Bot.subscription(ctx.bot, ctx.invitee) == nil

      assert {:ok, invitation} =
               Invitation.respond(ctx.invitation, true, ctx.invitee)

      assert Bot.subscription(ctx.bot, ctx.invitee) == :subscribed
      clear_expected_notifications(1)
    end

    test "Inviter receives a push notification on acceptance", ctx do
      assert {:ok, invitation} =
               Invitation.respond(ctx.invitation, true, ctx.invitee)

      msgs = Sandbox.wait_notifications(count: 1, timeout: 500, global: true)
      assert length(msgs) == 1

      assert %Notification{
               payload: %{
                 "aps" => %{"alert" => message}
               }
             } = hd(msgs)

      assert message ==
               "@#{ctx.invitee.handle} accepted your invitation to #{
                 ctx.bot.title
               }"

      clear_expected_notifications(1)
    end

    test "Invitee can decline", ctx do
      assert {:ok, invitation} =
               Invitation.respond(ctx.invitation, false, ctx.invitee)

      assert invitation.accepted == false

      assert no_more_push_notifications()
    end

    test "Invitee does not become subscribed if they decline", ctx do
      assert Bot.subscription(ctx.bot, ctx.invitee) == nil

      assert {:ok, invitation} =
               Invitation.respond(ctx.invitation, false, ctx.invitee)

      assert Bot.subscription(ctx.bot, ctx.invitee) == nil
      assert no_more_push_notifications()
    end

    test "Inviter cannot respond", ctx do
      assert {:error, :permission_denied} =
               Invitation.respond(ctx.invitation, false, ctx.user)

      assert no_more_push_notifications()
    end

    test "Other user cannot respond", ctx do
      assert {:error, :permission_denied} =
               Invitation.respond(
                 ctx.invitation,
                 false,
                 Factory.insert(:user)
               )

      assert no_more_push_notifications()
    end
  end

  describe "delete/2" do
    setup %{user: user, invitee: invitee} do
      invitation = Factory.insert(:invitation, user: user, invitee: invitee)
      invitation2 = Factory.insert(:invitation, invitee: invitee)
      Sandbox.wait_notifications(count: 2, timeout: 500, global: true)

      Invitation.delete(user, invitee)

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
