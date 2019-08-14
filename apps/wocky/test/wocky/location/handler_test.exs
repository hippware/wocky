defmodule Wocky.Location.HandlerTest do
  use Wocky.DataCase, async: false

  alias Wocky.Location.Handler
  alias Wocky.Relation
  alias Wocky.Repo.Factory
  alias Wocky.Roster

  describe "get_handler/1" do
    test "both versions should get the same handler" do
      user = Factory.insert(:user)

      assert Handler.get_handler(user.id) == Handler.get_handler(user)
    end
  end

  describe "bot subscription hooks" do
    setup do
      owner = Factory.insert(:user)
      user = Factory.insert(:user)

      bot = Factory.insert(:bot, user: owner)

      Roster.befriend(owner, user)

      pid = Handler.get_handler(user)

      Relation.subscribe(user, bot)

      {:ok, pid: pid, user: user, bot: bot}
    end

    test "should add a new bot subscription", %{bot: bot, pid: pid} do
      assert %{bot_subscriptions: [^bot]} = :sys.get_state(pid)
    end

    test "should remove a bot subscription", %{user: user, bot: bot, pid: pid} do
      Relation.unsubscribe(user, bot)

      assert %{bot_subscriptions: []} = :sys.get_state(pid)
    end
  end
end
