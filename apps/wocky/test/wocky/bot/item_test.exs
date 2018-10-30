defmodule Wocky.Bot.ItemTest do
  use Wocky.DataCase, async: true

  alias Faker.Lorem
  alias Wocky.Bot
  alias Wocky.Bot.Item
  alias Wocky.Repo.Factory
  alias Wocky.Repo.ID

  @image_stanza """
  <entry xmlns='some_namespace'>
  <content>blah blah</content>
  <image>some_image</image>
  </entry>
  """

  @no_image_stanza """
  <entry xmlns='some_namespace'>
  <content>blah blah</content>
  </entry>
  """

  describe "validation" do
    setup do
      valid_attrs = %{
        bot_id: ID.new(),
        user_id: ID.new(),
        id: ID.new(),
        stanza: "testing"
      }

      {:ok, valid_attrs: valid_attrs}
    end

    test "should pass with valid attributes", ctx do
      assert Item.changeset(%Item{}, ctx.valid_attrs).valid?
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

    test "get/1", %{bot: bot} do
      assert length(Item.get(bot)) == 2
      assert Item.get(Factory.build(:bot)) == []
    end

    test "get_count/1", %{bot: bot} do
      Factory.insert(:item, bot: bot, image: true)
      Factory.insert(:item, bot: bot, image: false)

      assert Item.get_count(bot) == 4
    end

    test "get_images/1", %{bot: bot, owner: owner} do
      Factory.insert(:item, bot: bot, user: owner, image: true)

      assert length(Item.get_images(bot)) == 1
    end

    test "get_image_count/1", %{bot: bot, owner: owner} do
      Factory.insert(:item, bot: bot, user: owner, image: true)

      assert Item.get_image_count(bot) == 1
    end

    test "get/2", %{id: id, bot: bot} do
      assert Item.get(id, bot)
      refute Item.get(id, Factory.build(:bot))
      refute Item.get(ID.new(), bot)
    end

    test "put/4 when an item does not already exist", ctx do
      assert {:ok, %Item{id: new_id}} =
               Item.put(ID.new(), ctx.bot, ctx.owner, "testing")

      assert Item.get(new_id, ctx.bot)

      bot = Repo.get(Bot, ctx.bot.id)
      assert DateTime.compare(bot.updated_at, ctx.bot.updated_at) == :gt
    end

    test "put/4 when an item already exists", ctx do
      assert {:ok, %Item{}} = Item.put(ctx.id, ctx.bot, ctx.owner, "testing")

      item = Item.get(ctx.id, ctx.bot)
      assert item.stanza == "testing"
      refute item.image

      bot = Repo.get(Bot, ctx.bot.id)
      assert DateTime.compare(bot.updated_at, ctx.bot.updated_at) == :gt
    end

    test "put/4 when a non-UUID id is supplied", ctx do
      assert {:ok, %Item{id: id}} =
               Item.put(Lorem.word(), ctx.bot, ctx.owner, "testing")

      assert ID.valid?(id)
    end

    test "put/4 with invalid input", ctx do
      assert {:error, _} =
               Item.put(ID.new(), Factory.build(:bot), ctx.author, Lorem.word())

      assert {:error, _} =
               Item.put(ID.new(), ctx.bot, Factory.build(:user), Lorem.word())
    end

    test "put/4 should set image to true when an image is present", ctx do
      assert {:ok, %Item{} = item} =
               Item.put(ID.new(), ctx.bot, ctx.owner, @image_stanza)

      assert item.image
    end

    test "permissions", ctx do
      user = Factory.insert(:user)
      item = Factory.insert(:item, user: user, bot: ctx.bot)

      # should refuse to publish an item that already exists
      # and is owned by another user
      assert Item.put(item.id, ctx.bot, ctx.owner, Lorem.paragraph()) ==
               {:error, :permission_denied}

      # should allow publication (update) of an item that already exists
      # and is owned by the same user
      assert {:ok, _} = Item.put(item.id, ctx.bot, user, Lorem.paragraph())
    end

    test "delete/1", ctx do
      assert Item.delete(ctx.bot) == :ok

      refute Item.get(ctx.id, ctx.bot)

      # should return :ok when the bot doesn't exist
      assert Item.delete(Factory.build(:bot)) == :ok
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

  describe "image detection" do
    test "has_image/1" do
      assert Item.has_image(@image_stanza)
      refute Item.has_image(@no_image_stanza)
      refute Item.has_image(Lorem.paragraph())
    end

    test "get_image/1" do
      assert Item.get_image(@image_stanza) == "some_image"

      refute Item.get_image(@no_image_stanza)
      refute Item.get_image(Lorem.paragraph())
    end
  end
end
