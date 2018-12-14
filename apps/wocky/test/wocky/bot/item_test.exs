defmodule Wocky.Bot.ItemTest do
  use Wocky.DataCase, async: true

  alias Faker.Lorem
  alias Wocky.Bot
  alias Wocky.Bot.Item
  alias Wocky.Repo.Factory
  alias Wocky.Repo.ID

  describe "validation" do
    setup do
      valid_attrs = %{
        bot_id: ID.new(),
        user_id: ID.new(),
        id: ID.new(),
        content: "testing"
      }

      {:ok, valid_attrs: valid_attrs}
    end

    test "should pass with valid attributes and content", ctx do
      assert Item.changeset(%Item{}, ctx.valid_attrs).valid?
    end

    test "should pass with valid attributes and image_url", ctx do
      attrs =
        ctx.valid_attrs
        |> Map.delete(:content)
        |> Map.put(:image_url, "testing")

      assert Item.changeset(%Item{}, attrs).valid?
    end

    test "should pass with both content and image_url", ctx do
      attrs = Map.put(ctx.valid_attrs, :image_url, "testing")

      assert Item.changeset(%Item{}, attrs).valid?
    end

    test "should fail with missing attributes", ctx do
      changeset = Item.changeset(%Item{}, %{})
      errors = errors_on(changeset)

      for attr <- Map.keys(ctx.valid_attrs) do
        assert Map.has_key?(errors, attr)
      end
    end

    test "foreign key error when the bot does not exist", ctx do
      assert {:error, changeset} =
               Repo.insert(Item.changeset(%Item{}, ctx.valid_attrs))

      assert errors_on(changeset).bot_id == ["does not exist"]
    end
  end

  describe "database operations" do
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

    test "get/2", %{id: id, bot: bot} do
      assert Item.get(id, bot)
      refute Item.get(id, Factory.build(:bot))
      refute Item.get(ID.new(), bot)
    end

    test "put/5 when an item does not already exist", ctx do
      assert {:ok, %Item{id: new_id}} =
               Item.put(ID.new(), ctx.bot, ctx.owner, "testing", nil)

      assert Item.get(new_id, ctx.bot)

      bot = Repo.get(Bot, ctx.bot.id)
      assert DateTime.compare(bot.updated_at, ctx.bot.updated_at) == :gt
    end

    test "put/5 when an item already exists", ctx do
      assert {:ok, %Item{}} =
               Item.put(ctx.id, ctx.bot, ctx.owner, "testing", "image")

      item = Item.get(ctx.id, ctx.bot)
      assert item.content == "testing"
      assert item.image_url == "image"

      bot = Repo.get(Bot, ctx.bot.id)
      assert DateTime.compare(bot.updated_at, ctx.bot.updated_at) == :gt
    end

    test "put/5 when a non-UUID id is supplied", ctx do
      assert {:ok, %Item{id: id}} =
               Item.put(Lorem.word(), ctx.bot, ctx.owner, "testing", nil)

      assert ID.valid?(id)
    end

    test "put/5 with invalid input", ctx do
      assert {:error, _} =
               Item.put(
                 ID.new(),
                 Factory.build(:bot),
                 ctx.author,
                 Lorem.word(),
                 nil
               )

      assert {:error, _} =
               Item.put(
                 ID.new(),
                 ctx.bot,
                 Factory.build(:user),
                 Lorem.word(),
                 nil
               )
    end

    test "permissions", ctx do
      user = Factory.insert(:user)
      item = Factory.insert(:item, user: user, bot: ctx.bot)

      # should refuse to publish an item that already exists
      # and is owned by another user
      assert Item.put(item.id, ctx.bot, ctx.owner, Lorem.paragraph(), nil) ==
               {:error, :permission_denied}

      # should allow publication (update) of an item that already exists
      # and is owned by the same user
      assert {:ok, _} = Item.put(item.id, ctx.bot, user, Lorem.paragraph(), nil)
    end

    test "delete/2", ctx do
      assert Item.delete(ctx.bot, ctx.author) == :ok

      refute Item.get(ctx.item2.id, ctx.bot)

      # should return :ok when the user doesn't exist
      assert Item.delete(ctx.bot, Factory.build(:user)) == :ok
    end

    test "delete/3 when the item doesn't exist", ctx do
      bad_bot = Factory.build(:bot)

      assert Item.delete(ctx.id, bad_bot, ctx.owner) == {:error, :not_found}
      assert Item.delete(ID.new(), ctx.bot, ctx.owner) == {:error, :not_found}
    end

    test "delete/3 when an item exists", ctx do
      assert Item.delete(ctx.item2.id, ctx.bot, ctx.author) == :ok
      refute Item.get(ctx.item2.id, ctx.bot)
    end

    test "delete/3 when an item exists and the bot owner deletes it", ctx do
      assert Item.delete(ctx.item2.id, ctx.bot, ctx.owner) == :ok
      refute Item.get(ctx.item2.id, ctx.bot)
    end

    test "delete/3 when an item exists and another user deletes it", ctx do
      assert Item.delete(ctx.id, ctx.bot, ctx.author) ==
               {:error, :permission_denied}

      assert Item.get(ctx.id, ctx.bot)
    end
  end
end
