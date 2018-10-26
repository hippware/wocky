defmodule Wocky.Bot.SubscriptionTest do
  use Wocky.DataCase, async: true

  alias Ecto.Adapters.SQL
  alias Wocky.Bot.Subscription
  alias Wocky.Repo
  alias Wocky.Repo.{Factory, ID}

  setup do
    [user, owner, visitor] = Factory.insert_list(3, :user)
    bot = Factory.insert(:bot, user: owner)
    Factory.insert(:subscription, user: user, bot: bot)

    Factory.insert(
      :subscription,
      user: visitor,
      bot: bot,
      visitor: true
    )

    {:ok, owner: owner, user: user, visitor: visitor, bot: bot}
  end

  defp valid_attrs, do: %{bot_id: ID.new(), user_id: ID.new()}

  describe "validation" do
    test "should pass with valid attributes" do
      changeset = Subscription.changeset(%Subscription{}, valid_attrs())
      assert changeset.valid?
    end

    test "should fail with missing attributes" do
      changeset = Subscription.changeset(%Subscription{}, %{})
      refute changeset.valid?

      assert errors_on(changeset).bot_id
      assert errors_on(changeset).user_id
    end

    test "foreign key error when the user does not exist" do
      {:error, changeset} =
        Repo.insert(Subscription.changeset(%Subscription{}, valid_attrs()))

      assert errors_on(changeset).user_id == ["does not exist"]
    end
  end

  describe "state/2" do
    test "should return :subscribed if the user is subscribed to the bot",
         ctx do
      assert Subscription.state(ctx.user, ctx.bot) == :subscribed
    end

    test "should return nil when the user does not exist", ctx do
      user = Factory.build(:user, resource: "testing")
      refute Subscription.state(user, ctx.bot)
    end

    test "should return nil when the bot does not exist", ctx do
      bot = Factory.build(:bot)
      refute Subscription.state(ctx.user, bot)
    end

    test "should return nil when the user is not subscribed to the bot", ctx do
      refute Subscription.state(ctx.owner, ctx.bot)
    end

    test "should return :visitor when the user is a visitor", ctx do
      assert Subscription.state(ctx.visitor, ctx.bot) == :visiting
    end
  end

  describe "get/2" do
    test "should return the subscription", ctx do
      assert Subscription.get(ctx.user, ctx.bot)
    end

    test "should return nil when the user does not exist", ctx do
      user = Factory.build(:user)
      refute Subscription.get(user, ctx.bot)
    end

    test "should return nil when the bot does not exist", ctx do
      bot = Factory.build(:bot)
      refute Subscription.get(ctx.user, bot)
    end

    test "should return nil when the user is not subscribed to the bot", ctx do
      refute Subscription.get(ctx.owner, ctx.bot)
    end
  end

  describe "put/2" do
    test "when a subscription does not already exist", ctx do
      new_user = Factory.insert(:user)
      result = Subscription.put(new_user, ctx.bot)

      assert result == :ok
      assert Subscription.state(new_user, ctx.bot) == :subscribed
    end

    test "when a subscription already exists", ctx do
      result = Subscription.put(ctx.visitor, ctx.bot)

      assert result == :ok
      assert Subscription.state(ctx.visitor, ctx.bot) == :visiting
    end
  end

  describe "delete/2" do
    test "when a subscription exists", ctx do
      result = Subscription.delete(ctx.user, ctx.bot)

      assert result == :ok
      refute Subscription.state(ctx.user, ctx.bot)
    end

    test "when the bot owner is trying to unsubscribe", ctx do
      assert {:error, _} = Subscription.delete(ctx.owner, ctx.bot)
    end

    test "when the user doesn't exist", ctx do
      user = Factory.build(:user)

      assert Subscription.delete(user, ctx.bot) == :ok
    end

    test "when the bot doesn't exist", ctx do
      bot = Factory.build(:bot)

      assert Subscription.delete(ctx.user, bot) == :ok
    end
  end

  describe "visit/2" do
    test "should set the subscriber as a visitor", ctx do
      assert Subscription.visit(ctx.user, ctx.bot) == :ok
      assert Subscription.state(ctx.user, ctx.bot) == :visiting
    end
  end

  describe "depart/2" do
    test "should set the visitor as a subscriber", ctx do
      assert Subscription.depart(ctx.visitor, ctx.bot) == :ok
      assert Subscription.state(ctx.visitor, ctx.bot) == :subscribed
    end
  end

  describe "is_subscribed/2 stored procedure" do
    test "should return true if the user is subscribed to the bot", ctx do
      assert is_subscribed_sp(ctx.user, ctx.bot)
    end

    test "should return false when the user does not exist", ctx do
      user = Factory.build(:user, resource: "testing")
      refute is_subscribed_sp(user, ctx.bot)
    end

    test "should return false when the bot does not exist", ctx do
      bot = Factory.build(:bot)
      refute is_subscribed_sp(ctx.user, bot)
    end

    test "should return false when the user is not subscribed to the bot",
         ctx do
      refute is_subscribed_sp(ctx.owner, ctx.bot)
    end
  end

  defp is_subscribed_sp(user, bot) do
    {:ok, u} = Ecto.UUID.dump(user.id)
    {:ok, b} = Ecto.UUID.dump(bot.id)

    Repo
    |> SQL.query!("SELECT is_subscribed($1, $2)", [u, b])
    |> Map.get(:rows)
    |> hd
    |> hd
  end
end
