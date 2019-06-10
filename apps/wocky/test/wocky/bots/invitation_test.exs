defmodule Wocky.Bots.InvitationTest do
  use Wocky.DataCase, async: true

  alias Wocky.Bots
  alias Wocky.Bots.Invitation
  alias Wocky.Repo.Factory
  alias Wocky.Roster

  setup do
    [user, invitee, stranger] = Factory.insert_list(3, :user, device: "testing")
    Roster.befriend(user, invitee)
    bot = Factory.insert(:bot, user: user)

    {:ok, user: user, invitee: invitee, stranger: stranger, bot: bot}
  end

  describe "put/3" do
    test "should create an invitation", ctx do
      assert {:ok, invitation} = Invitation.put(ctx.invitee, ctx.bot, ctx.user)

      assert invitation == Repo.get_by(Invitation, id: invitation.id)
      assert invitation.accepted == nil
    end

    test "refuse invitation to non-owned bot", ctx do
      other_user = Factory.insert(:user)

      assert {:error, :permission_denied} ==
               Invitation.put(ctx.invitee, ctx.bot, other_user)
    end

    test "refuse invitation to non-friend", ctx do
      assert {:error, :permission_denied} =
               Invitation.put(ctx.stranger, ctx.bot, ctx.user)
    end

    test "subsequent invitations should overwrite existing ones", ctx do
      assert {:ok, invitation} = Invitation.put(ctx.invitee, ctx.bot, ctx.user)

      assert {:ok, invitation2} = Invitation.put(ctx.invitee, ctx.bot, ctx.user)

      assert invitation.id == invitation2.id

      assert DateTime.compare(invitation.updated_at, invitation2.updated_at) ==
               :lt
    end
  end

  describe "get/2 by id" do
    setup :setup_invitation

    test "User can get their own invitation", ctx do
      assert %Invitation{} = Invitation.get(ctx.invitation.id, ctx.user)
    end

    test "Invitee can get their own invitation", ctx do
      assert %Invitation{} = Invitation.get(ctx.invitation.id, ctx.invitee)
    end

    test "Unrelated user cannot get the invitation", ctx do
      assert nil == Invitation.get(ctx.invitation.id, Factory.insert(:user))
    end
  end

  describe "get/2 by bot id" do
    setup :setup_invitation

    test "User can get their own invitation", ctx do
      assert %Invitation{} = Invitation.get(ctx.bot, ctx.user)
    end

    test "Invitee can get their own invitation", ctx do
      assert %Invitation{} = Invitation.get(ctx.bot, ctx.invitee)
    end

    test "Unrelated user cannot get the invitation", ctx do
      assert nil == Invitation.get(ctx.bot, Factory.insert(:user))
    end
  end

  describe "exists?/2 - true" do
    setup :setup_invitation

    test "exists by id", ctx do
      assert Invitation.exists?(ctx.invitation.id, ctx.user)
    end

    test "exists by bot", ctx do
      assert Invitation.exists?(ctx.bot, ctx.user)
    end
  end

  describe "exists?/2 - false" do
    test "does not exist by id", ctx do
      refute Invitation.exists?(1, ctx.user)
    end

    test "does not exist by bot", ctx do
      refute Invitation.exists?(ctx.bot, ctx.user)
    end
  end

  describe "respond/3" do
    setup :setup_invitation

    test "Invitee can accept", ctx do
      assert {:ok, invitation} =
               Invitation.respond(ctx.invitation, true, ctx.invitee)

      assert invitation.accepted == true
    end

    test "Invitee becomes subscribed if they accept", ctx do
      assert Bots.subscription(ctx.bot, ctx.invitee) == nil

      assert {:ok, invitation} =
               Invitation.respond(ctx.invitation, true, ctx.invitee)

      assert Bots.subscription(ctx.bot, ctx.invitee) == :subscribed
    end

    test "Invitee can decline", ctx do
      assert {:ok, invitation} =
               Invitation.respond(ctx.invitation, false, ctx.invitee)

      assert invitation.accepted == false
    end

    test "Invitee does not become subscribed if they decline", ctx do
      assert Bots.subscription(ctx.bot, ctx.invitee) == nil

      assert {:ok, invitation} =
               Invitation.respond(ctx.invitation, false, ctx.invitee)

      assert Bots.subscription(ctx.bot, ctx.invitee) == nil
    end

    test "Inviter cannot respond", ctx do
      assert {:error, :permission_denied} =
               Invitation.respond(ctx.invitation, false, ctx.user)
    end

    test "Other user cannot respond", ctx do
      assert {:error, :permission_denied} =
               Invitation.respond(
                 ctx.invitation,
                 false,
                 Factory.insert(:user)
               )
    end
  end

  describe "delete/2" do
    setup %{user: user, invitee: invitee} do
      invitation = Factory.insert(:bot_invitation, user: user, invitee: invitee)
      invitation2 = Factory.insert(:bot_invitation, invitee: invitee)

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

  defp setup_invitation(ctx) do
    invitation =
      Factory.insert(:bot_invitation,
        user: ctx.user,
        invitee: ctx.invitee,
        bot: ctx.bot
      )

    {:ok, invitation: invitation}
  end
end
