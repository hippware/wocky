defmodule Wocky.User.Location.HandlerTest do
  use Wocky.DataCase

  alias Wocky.Bot
  alias Wocky.Repo.Factory
  alias Wocky.Roster
  alias Wocky.User.Location.Handler

  setup do
    owner = Factory.insert(:user)
    user = Factory.insert(:user)

    bot = Factory.insert(:bot, user: owner)

    Roster.befriend(owner, user)

    pid = Handler.get_handler(user)

    {:ok, pid: pid, user: user, bot: bot}
  end

  test "should add a new bot subscription", %{user: user, bot: bot, pid: pid} do
    Bot.subscribe(bot, user)

    assert %{subscriptions: [bot]} = :sys.get_state(pid)
  end

  test "should remove a bot subscription", %{user: user, bot: bot, pid: pid} do
    Bot.subscribe(bot, user)
    Bot.unsubscribe(bot, user)

    assert %{subscriptions: []} = :sys.get_state(pid)
  end
end
