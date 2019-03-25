defmodule Wocky.Roster.RosterNotificationTest do
  use Wocky.WatcherCase

  alias Faker.Code
  alias Pigeon.APNS.Notification
  alias Wocky.{Push, Roster}
  alias Wocky.Push.Backend.Sandbox
  alias Wocky.Repo.Factory

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

      assert %Notification{
               payload: %{
                 "aps" => %{"alert" => message}
               }
             } = hd(msgs)

      assert message == "@#{ctx.user2.handle} invited you to be their friend"
    end
  end
end
