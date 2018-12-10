defmodule Wocky.BotTest do
  use Wocky.DataCase, async: true

  use Wocky.RSMHelper

  alias Wocky.Bot
  alias Wocky.GeoUtils
  alias Wocky.Repo
  alias Wocky.Repo.{Factory, ID}
  alias Wocky.User

  setup do
    user = Factory.insert(:user)
    bot = Factory.insert(:bot, user: user)

    {:ok, user: user, bot: bot}
  end

  describe "validations" do
    setup do
      {:ok,
       attrs: %{
         id: ID.new(),
         user_id: ID.new(),
         title: "test bot",
         location: GeoUtils.point(5.0, 5.0)
       }}
    end

    test "should pass with valid attributes", %{attrs: attrs} do
      assert Bot.changeset(%Bot{}, attrs).valid?
    end

    test "should set pending to 'false'", %{attrs: attrs} do
      changeset = Bot.changeset(%Bot{}, attrs)
      refute changeset.changes.pending
    end

    test "should fail with missing fields", %{attrs: attrs} do
      changeset = Bot.changeset(%Bot{}, %{})

      refute changeset.valid?

      errors = Map.keys(errors_on(changeset))

      for {attr, _} <- attrs do
        assert Enum.member?(errors, attr)
      end
    end

    test "should fail with negative radius", %{attrs: attrs} do
      changeset = Bot.changeset(%Bot{}, Map.put(attrs, :radius, -1))
      refute changeset.valid?
      assert errors_on(changeset).radius
    end

    test "should fail with a nil description", %{attrs: attrs} do
      changeset = Bot.changeset(%Bot{}, Map.put(attrs, :description, nil))
      refute changeset.valid?
      assert errors_on(changeset).description
    end
  end

  describe "get/2" do
    setup ctx do
      pending = Factory.insert(:bot, user: ctx.user, pending: true)

      {:ok, pending: pending}
    end

    test "should return the requested bot", %{bot: bot} do
      assert bot.id |> Bot.get() |> Repo.preload(:user) == bot
    end

    test "should return nil for non-existant bots" do
      refute Bot.get(ID.new())
    end

    test "should not return pending bots by default", %{pending: pending} do
      refute Bot.get(pending.id)
    end

    test "should return pending bots if specified", %{pending: pending} do
      assert pending.id |> Bot.get(true) |> Repo.preload(:user) == pending
    end
  end

  describe "preallocate/2" do
    setup ctx do
      preallocated = Bot.preallocate(ctx.user.id)

      {:ok, preallocated: preallocated}
    end

    test "returns a pending bot", %{preallocated: preallocated} do
      assert preallocated.pending
    end

    test "creates a bot in the database", %{preallocated: preallocated} do
      db_bot = Repo.get(Bot, preallocated.id)

      assert db_bot.user_id == preallocated.user_id
    end

    test "raises on error" do
      assert_raise Ecto.InvalidChangesetError, fn ->
        Bot.preallocate(ID.new())
      end
    end
  end

  describe "insert/1" do
    test "returns an ok result on success", %{user: user} do
      bot_params = Factory.params_for(:bot, user: user)

      assert {:ok, _} = Bot.insert(bot_params)
    end

    test "returns an error result on failure" do
      assert {:error, _} = Bot.insert(%{})
    end
  end

  describe "update/2" do
    test "returns an ok result on success", %{bot: bot} do
      assert {:ok, _} = Bot.update(bot, %{title: "updated bot"})
    end

    test "returns an error result on failure" do
      assert {:error, _} = Bot.update(%Bot{}, %{})
    end

    test "should normalize latitude and longitude", %{bot: bot} do
      {:ok, %Bot{id: id}} =
        Bot.update(bot, %{location: GeoUtils.point(-95.0, -185)})

      assert Repo.get(Bot, id).location == GeoUtils.point(-85, 175)
    end
  end

  describe "delete/1" do
    setup ctx do
      result = Bot.delete(ctx.bot)

      {:ok, result: result}
    end

    test "should retun :ok", %{result: result} do
      assert result == :ok
    end

    test "should remove the bot", %{bot: bot} do
      refute Repo.get(Bot, bot.id)
    end
  end

  test "owner/1", %{bot: bot, user: user} do
    assert Bot.owner(bot) == user
  end

  describe "subscribers" do
    setup ctx do
      sub = Factory.insert(:user)
      Bot.subscribe(ctx.bot, sub)

      {:ok, sub: sub}
    end

    test "subscribers/1", %{bot: bot, user: user} do
      subscribers = Bot.subscribers(bot)

      assert length(subscribers) == 1
      assert %User{} = hd(subscribers)
      refute Enum.member?(subscribers, user)
    end

    test "subscribers_query/1", %{bot: bot, user: user} do
      subscribers = bot |> Bot.subscribers_query() |> Repo.all()

      assert length(subscribers) == 1
      assert %User{} = hd(subscribers)
      refute Enum.member?(subscribers, user)
    end

    test "subscriber_count/1", %{bot: bot} do
      assert Bot.subscriber_count(bot) == 1
    end
  end

  describe "is_visible_query/2" do
    setup do
      [user1, user2] = Factory.insert_list(2, :user)
      owned_bot = Factory.insert(:bot, user: user1)
      invited_bot = Factory.insert(:bot, user: user2)

      Factory.insert(:invitation,
        invitee: user1,
        bot: invited_bot,
        user: user2
      )

      private_bot = Factory.insert(:bot, user: user2)
      pending_bot = Factory.insert(:bot, user: user1, pending: true)
      subscribed_bot = Factory.insert(:bot, user: user2)
      Factory.insert(:subscription, bot: subscribed_bot, user: user1)

      {:ok,
       user: user1,
       owned_bot: owned_bot,
       invited_bot: invited_bot,
       private_bot: private_bot,
       pending_bot: pending_bot,
       subscribed_bot: subscribed_bot}
    end

    test "should allow owned bots", ctx do
      assert run_is_visible_query(ctx.owned_bot, ctx.user) == ctx.owned_bot
    end

    test "should allow invited bots", ctx do
      assert run_is_visible_query(ctx.invited_bot, ctx.user) == ctx.invited_bot
    end

    test "should refuse private bots", ctx do
      refute run_is_visible_query(ctx.private_bot, ctx.user)
    end

    test "should allow subscribed bots", ctx do
      assert run_is_visible_query(ctx.subscribed_bot, ctx.user) ==
               ctx.subscribed_bot
    end
  end

  defp run_is_visible_query(bot, user) do
    Bot
    |> where(id: ^bot.id)
    |> Bot.is_visible_query(user)
    |> preload(:user)
    |> Repo.one()
  end
end
