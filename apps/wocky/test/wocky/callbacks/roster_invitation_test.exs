defmodule Wocky.Callbacks.RosterInvitationTest do
  use Wocky.WatcherCase

  alias Faker.Code
  alias Pigeon.APNS.Notification, as: APNSNotification
  alias Wocky.Callbacks.RosterInvitation, as: Callback
  alias Wocky.Notifier.InBand.Notification
  alias Wocky.Notifier.Push
  alias Wocky.Notifier.Push.Backend.Sandbox
  alias Wocky.Repo
  alias Wocky.Repo.Factory
  alias Wocky.Roster

  setup_all do
    Callback.register()
  end

  setup do
    [user1, user2] = Factory.insert_list(2, :user, device: "testing")
    Sandbox.clear_notifications(global: true)

    {:ok, user1: user1, user2: user2}
  end

  describe "invitation notification" do
    setup ctx do
      token = Code.isbn13()
      :ok = Push.enable(ctx.user1, ctx.user1.device, token)
    end

    test "invite user", ctx do
      Roster.invite(ctx.user2, ctx.user1)

      msgs = Sandbox.wait_notifications(count: 1, timeout: 10_000, global: true)
      assert length(msgs) == 1

      assert %APNSNotification{
               payload: %{
                 "aps" => %{"alert" => message}
               }
             } = hd(msgs)

      assert message == "@#{ctx.user2.handle} wants to connect with you."

      assert_eventually(Repo.get_by(Notification, user_id: ctx.user1.id) != nil)
    end
  end
end
