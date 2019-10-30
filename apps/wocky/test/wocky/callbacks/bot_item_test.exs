defmodule Wocky.Callbacks.BotItemTest do
  use Wocky.WatcherCase

  import Wocky.PushHelper

  alias Faker.Code
  alias Faker.Lorem
  alias Pigeon.APNS.Notification
  alias Wocky.Callbacks.BotItem, as: Callback
  alias Wocky.Friends
  alias Wocky.Notifier.Push
  alias Wocky.Notifier.Push.Backend.Sandbox
  alias Wocky.POI
  alias Wocky.Relation
  alias Wocky.Repo.Factory

  setup_all do
    Callback.register()
  end

  setup do
    [user, author, sub] = Factory.insert_list(3, :user, device: "testing")
    bot = Factory.insert(:bot, user: user)
    Friends.befriend(user, author)
    Friends.befriend(user, sub)
    Relation.subscribe(author, bot)
    Relation.subscribe(sub, bot)

    Sandbox.clear_notifications(global: true)

    :ok = Push.enable(sub, user.device, Code.isbn13())

    {:ok, user: user, author: author, sub: sub, bot: bot}
  end

  describe "insert" do
    test "should trigger a notification", ctx do
      {:ok, _} = POI.put_item(ctx.bot, nil, Lorem.sentence(), nil, ctx.author)

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
      {:ok, _} = POI.put_item(ctx.bot, nil, Lorem.sentence(), nil, ctx.sub)

      assert no_more_push_notifications()
    end
  end
end
