defmodule Wocky.Bot.ItemNotificationTest do
  use Wocky.WatcherHelper

  alias Faker.{Code, Lorem}
  alias Pigeon.APNS.Notification
  alias Wocky.Bot
  alias Wocky.Bot.Item
  alias Wocky.Push
  alias Wocky.Push.Sandbox
  alias Wocky.Repo.Factory

  setup do
    [user, author, sub] = Factory.insert_list(3, :user, resource: "testing")
    bot = Factory.insert(:bot, user: user)
    Bot.subscribe(bot, author)
    Bot.subscribe(bot, sub)

    Sandbox.clear_notifications(global: true)

    :ok = Push.enable(sub.id, user.resource, Code.isbn13())

    {:ok, user: user, author: author, sub: sub, bot: bot}
  end

  describe "put/3" do
    test "should trigger a notification", ctx do
      {:ok, _} = Item.put(nil, ctx.bot, ctx.author, Lorem.sentence())

      msgs = Sandbox.wait_notifications(count: 1, timeout: 500, global: true)
      assert length(msgs) == 1

      assert %Notification{
               payload: %{
                 "aps" => %{"alert" => message}
               }
             } = hd(msgs)

      assert message == "@#{ctx.author.handle} commented on #{ctx.bot.title}"

      clear_expected_notifications(1)
    end

    test "should not trigger a notification to the author", ctx do
      {:ok, _} = Item.put(nil, ctx.bot, ctx.sub, Lorem.sentence())

      assert no_more_push_notifications()
    end
  end
end
