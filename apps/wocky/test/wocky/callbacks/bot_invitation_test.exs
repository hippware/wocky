defmodule Wocky.Callbacks.BotInvitationTest do
  use Wocky.WatcherCase

  import Wocky.PushHelper

  alias Faker.Code
  alias Pigeon.APNS.Notification
  alias Wocky.Callbacks.BotInvitation, as: Callback
  alias Wocky.Contacts
  alias Wocky.Notifier.InBand
  alias Wocky.Notifier.Push
  alias Wocky.Notifier.Push.Backend.Sandbox
  alias Wocky.Relation
  alias Wocky.Repo.Factory

  setup_all do
    Callback.register()
  end

  setup do
    [user, invitee] = Factory.insert_list(2, :user, device: "testing")
    Contacts.befriend(user, invitee)
    bot = Factory.insert(:bot, user: user)

    Sandbox.clear_notifications(global: true)

    :ok = Push.enable(invitee, invitee.device, Code.isbn13())
    :ok = Push.enable(user, user.device, Code.isbn13())

    {:ok, user: user, invitee: invitee, bot: bot}
  end

  describe "sending an invitation" do
    test "should send a notification", ctx do
      assert {:ok, invitation} = Relation.invite(ctx.invitee, ctx.bot, ctx.user)

      msgs = Sandbox.wait_notifications(count: 1, timeout: 500, global: true)
      assert length(msgs) == 1

      assert %Notification{
               payload: %{
                 "aps" => %{"alert" => message}
               }
             } = hd(msgs)

      assert message ==
               "@#{ctx.user.handle} invited you to follow #{ctx.bot.title}"

      assert clear_expected_notifications(1)
    end

    test "subsequent invitations should overwrite existing ones", ctx do
      assert {:ok, _} = Relation.invite(ctx.invitee, ctx.bot, ctx.user)
      assert {:ok, _} = Relation.invite(ctx.invitee, ctx.bot, ctx.user)

      assert_eventually(in_band_notifications(ctx.invitee) == 1)
      assert in_band_notifications(ctx.user) == 0

      assert clear_expected_notifications(1)
    end
  end

  describe "responding to an invitation" do
    setup ctx do
      invitation =
        Factory.insert(:bot_invitation,
          user: ctx.user,
          invitee: ctx.invitee,
          bot: ctx.bot
        )

      clear_expected_notifications(1)

      {:ok, invitation: invitation}
    end

    test "Inviter receives a push notification on acceptance", ctx do
      assert {:ok, invitation} =
               Relation.respond(ctx.invitation, true, ctx.invitee)

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

    test "Inviter does not recieve a notification if invitee declines", ctx do
      assert {:ok, invitation} =
               Relation.respond(ctx.invitation, false, ctx.invitee)

      assert no_more_push_notifications()
    end
  end

  defp in_band_notifications(user),
    do: user |> InBand.user_query(nil, nil) |> Repo.all() |> length()
end
