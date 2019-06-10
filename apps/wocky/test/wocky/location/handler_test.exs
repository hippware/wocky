defmodule Wocky.Location.HandlerTest do
  use Wocky.DataCase, async: false

  alias Wocky.Bots
  alias Wocky.Location.Handler
  alias Wocky.Repo.Factory
  alias Wocky.Roster

  setup do
    owner = Factory.insert(:user)
    user = Factory.insert(:user)

    bot = Factory.insert(:bot, user: owner)

    Roster.befriend(owner, user)

    pid = Handler.get_handler(user)

    Bots.subscribe(bot, user)

    {:ok, pid: pid, user: user, bot: bot}
  end

  describe "bot subscription hooks" do
    test "should add a new bot subscription", %{bot: bot, pid: pid} do
      assert %{subscriptions: [^bot]} = :sys.get_state(pid)
    end

    test "should remove a bot subscription", %{user: user, bot: bot, pid: pid} do
      Bots.unsubscribe(bot, user)

      assert %{subscriptions: []} = :sys.get_state(pid)
    end
  end
end
