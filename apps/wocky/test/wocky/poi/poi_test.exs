defmodule Wocky.POI.POITest do
  use Wocky.DataCase, async: true

  alias Faker.Lorem
  alias Wocky.GeoUtils
  alias Wocky.POI
  alias Wocky.POI.Bot
  alias Wocky.POI.Item
  alias Wocky.Repo
  alias Wocky.Repo.Factory
  alias Wocky.Repo.ID

  setup do
    user = Factory.insert(:user)
    bot = Factory.insert(:bot, user: user)

    {:ok, user: user, bot: bot}
  end

  describe "get/2" do
    setup ctx do
      pending = Factory.insert(:bot, user: ctx.user, pending: true)

      {:ok, pending: pending}
    end

    test "should return the requested bot", %{bot: bot} do
      assert bot.id |> POI.get() |> Repo.preload(:user) == bot
    end

    test "should return nil for non-existant bots" do
      refute POI.get(ID.new())
    end

    test "should not return pending bots by default", %{pending: pending} do
      refute POI.get(pending.id)
    end

    test "should return pending bots if specified", %{pending: pending} do
      assert pending.id |> POI.get(true) |> Repo.preload(:user) == pending
    end
  end

  describe "preallocate/2" do
    setup ctx do
      {:ok, preallocated} = POI.preallocate(ctx.user)

      {:ok, preallocated: preallocated}
    end

    test "returns a pending bot", %{preallocated: preallocated} do
      assert preallocated.pending
    end

    test "creates a bot in the database", %{preallocated: preallocated} do
      db_bot = Repo.get(Bot, preallocated.id)

      assert db_bot.user_id == preallocated.user_id
    end

    test "should return an error when the user doesn't exist" do
      assert {:error, _} = POI.preallocate(Factory.build(:user))
    end
  end

  describe "insert/1" do
    test "returns an ok result on success", %{user: user} do
      bot_params = Factory.params_for(:bot, user: user)

      assert {:ok, _} = POI.insert(bot_params, user)
    end

    test "returns an error result on failure", %{user: user} do
      assert {:error, _} = POI.insert(%{}, user)
    end
  end

  describe "update/2" do
    test "returns an ok result on success", %{bot: bot} do
      assert {:ok, _} = POI.update(bot, %{title: "updated bot"})
    end

    test "returns an error result on failure" do
      assert {:error, _} = POI.update(%Bot{}, %{})
    end

    test "should normalize latitude and longitude", %{bot: bot} do
      {:ok, %Bot{id: id}} =
        POI.update(bot, %{location: GeoUtils.point(-95.0, -185)})

      assert Repo.get(Bot, id).location == GeoUtils.point(-85, 175)
    end
  end

  describe "delete/1" do
    setup ctx do
      result = POI.delete(ctx.bot)

      {:ok, result: result}
    end

    test "should retun :ok", %{result: result} do
      assert result == :ok
    end

    test "should remove the bot", %{bot: bot} do
      refute Repo.get(Bot, bot.id)
    end
  end

  describe "bot items" do
    setup do
      [owner, author] = Factory.insert_list(2, :user)
      bot = Factory.insert(:bot, user: owner)
      item = Factory.insert(:item, bot: bot, user: owner)
      item2 = Factory.insert(:item, bot: bot, user: author)

      {:ok,
       owner: owner,
       author: author,
       bot: bot,
       id: item.id,
       item: item,
       item2: item2}
    end

    test "get_items/1", %{bot: bot} do
      assert bot |> POI.get_items() |> length() == 2

      new_bot = Factory.build(:bot)
      assert POI.get_items(new_bot) == []
    end

    test "get_item/2", %{id: id, bot: bot} do
      assert POI.get_item(bot, id)
      refute POI.get_item(Factory.build(:bot), id)
      refute POI.get_item(bot, ID.new())
    end

    test "put_item/5 when an item does not already exist", ctx do
      assert {:ok, %Item{id: new_id}} =
               POI.put_item(ctx.bot, ID.new(), "testing", nil, ctx.owner)

      assert POI.get_item(ctx.bot, new_id)

      bot = Repo.get(Bot, ctx.bot.id)
      assert DateTime.compare(bot.updated_at, ctx.bot.updated_at) == :gt
    end

    test "put_item/5 when an item already exists", ctx do
      assert {:ok, %Item{}} =
               POI.put_item(ctx.bot, ctx.id, "testing", "image", ctx.owner)

      item = POI.get_item(ctx.bot, ctx.id)
      assert item.content == "testing"
      assert item.image_url == "image"

      bot = Repo.get(Bot, ctx.bot.id)
      assert DateTime.compare(bot.updated_at, ctx.bot.updated_at) == :gt
    end

    test "put_item/5 when a non-UUID id is supplied", ctx do
      assert {:ok, %Item{id: id}} =
               POI.put_item(ctx.bot, Lorem.word(), "testing", nil, ctx.owner)

      assert ID.valid?(id)
    end

    test "put_item/5 with invalid input", ctx do
      assert {:error, _} =
               POI.put_item(
                 Factory.build(:bot),
                 ID.new(),
                 Lorem.word(),
                 nil,
                 ctx.author
               )

      assert {:error, _} =
               POI.put_item(
                 ctx.bot,
                 ID.new(),
                 Lorem.word(),
                 nil,
                 Factory.build(:user)
               )
    end

    test "put_item/5 when updating another user's item", ctx do
      user = Factory.insert(:user)
      item = Factory.insert(:item, user: user, bot: ctx.bot)
      content = Lorem.paragraph()

      # should refuse to publish an item that already exists
      # and is owned by another user
      assert POI.put_item(ctx.bot, item.id, content, nil, ctx.owner) ==
               {:error, :permission_denied}
    end

    test "delete_item/3 when the item doesn't exist", ctx do
      bad_bot = Factory.build(:bot)

      assert POI.delete_item(bad_bot, ctx.id, ctx.owner) ==
               {:error, :not_found}

      assert POI.delete_item(ctx.bot, ID.new(), ctx.owner) ==
               {:error, :not_found}
    end

    test "delete_item/3 when an item exists", ctx do
      assert POI.delete_item(ctx.bot, ctx.item2.id, ctx.author) == :ok
      refute POI.get_item(ctx.bot, ctx.item2.id)
    end

    test "delete_item/3 when an item exists and the bot owner deletes it",
         ctx do
      assert POI.delete_item(ctx.bot, ctx.item2.id, ctx.owner) == :ok
      refute POI.get_item(ctx.bot, ctx.item2.id)
    end

    test "delete_item/3 when an item exists and another user deletes it", ctx do
      assert POI.delete_item(ctx.bot, ctx.id, ctx.author) ==
               {:error, :permission_denied}

      assert POI.get_item(ctx.bot, ctx.id)
    end

    test "delete_items/2", ctx do
      assert POI.delete_items(ctx.bot, ctx.author) == :ok
      refute POI.get_item(ctx.bot, ctx.item2.id)

      # should return :ok when the user doesn't exist
      assert POI.delete_items(ctx.bot, Factory.build(:user)) == :ok
    end
  end
end
