defmodule Wocky.Bot.RosterTest do
  use Wocky.WatcherHelper

  alias Faker.Code
  alias Pigeon.APNS.Notification
  alias Wocky.{Push, Roster}
  alias Wocky.Push.Sandbox
  alias Wocky.Repo.Factory

  setup do
    [user1, user2] = Factory.insert_list(2, :user, resource: "testing")
    Sandbox.clear_notifications(global: true)

    {:ok, user1: user1, user2: user2}
  end

  describe "follow notification" do
    setup shared do
      token = Code.isbn13()
      :ok = Push.enable(shared.user1, shared.user1.resource, token)
    end

    test "start following", shared do
      Roster.follow(shared.user2.id, shared.user1.id)

      msgs = Sandbox.wait_notifications(count: 1, timeout: 10_000, global: true)
      assert length(msgs) == 1

      assert %Notification{
               payload: %{
                 "aps" => %{"alert" => message}
               }
             } = hd(msgs)

      assert message == "@#{shared.user2.handle} started following you"
    end
  end
end
