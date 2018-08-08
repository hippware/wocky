# credo:disable-for-this-file Credo.Check.Refactor.PipeChainStart
defmodule Wocky.Bot.ItemSpec do
  use ESpec, async: true
  use ModelHelpers

  alias Faker.Lorem
  alias Wocky.Bot
  alias Wocky.Bot.Item
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
    let :valid_attrs,
      do: %{
        bot_id: ID.new(),
        user_id: ID.new(),
        id: ID.new(),
        stanza: "testing"
      }

    it "should pass with valid attributes" do
      %Item{}
      |> Item.changeset(valid_attrs())
      |> should(be_valid())
    end

    it "should fail with missing attributes" do
      %Item{}
      |> Item.changeset(%{})
      |> should(have_errors [:bot_id, :user_id, :id, :stanza])
    end

    describe "converting foreign key constraints to errors" do
      let :changeset, do: Item.changeset(%Item{}, valid_attrs())

      it do: changeset() |> Repo.insert() |> should(be_error_result())

      context "when the bot does not exist" do
        let :new_changeset do
          {:error, changeset} = Repo.insert(changeset())
          changeset
        end

        it "has error" do
          error = {:bot_id, {"does not exist", []}}
          new_changeset().errors |> should(have error)
        end
      end
    end
  end

  describe "database operations" do
    before do
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

    describe "get/1" do
      it "should return a list of items" do
        Item.get(shared.bot) |> should(have_count 2)
      end

      it "should return an empty list when the bot does not exist" do
        Factory.build(:bot) |> Item.get() |> should(be_empty())
      end
    end

    describe "get_count/1" do
      before do
        Factory.insert(:item, bot: shared.bot, image: true)
        Factory.insert(:item, bot: shared.bot, image: false)
        :ok
      end

      it "should return total item count including images" do
        shared.bot |> Item.get_count() |> should(eq 4)
      end
    end

    describe "get_images/1" do
      before do
        Factory.insert(:item, bot: shared.bot, user: shared.owner, image: true)
        :ok
      end

      it "should return items that have the image flag set" do
        shared.bot |> Item.get_images() |> should(have_count 1)
      end
    end

    describe "get_image_count/1" do
      before do
        Factory.insert(:item, bot: shared.bot, user: shared.owner, image: true)
        :ok
      end

      it "should return items that have the image flag set" do
        shared.bot |> Item.get_image_count() |> should(eq 1)
      end
    end

    describe "get/2" do
      it "should return the item" do
        shared.id |> Item.get(shared.bot) |> should_not(be_nil())
      end

      it "should return nil when the bot does not exist" do
        shared.id |> Item.get(Factory.build(:bot)) |> should(be_nil())
      end

      it "should return nil when the id does not exist" do
        ID.new() |> Item.get(shared.bot) |> should(be_nil())
      end
    end

    describe "put/4" do
      context "when an item does not already exist" do
        before do
          new_id = ID.new()
          result = Item.put(new_id, shared.bot, shared.owner, "testing")
          {:ok, new_id: elem(result, 1).id, result: result}
        end

        it "should return {:ok, Item}" do
          shared.result |> should(be_ok_result())
          shared.result |> elem(1) |> should(be_struct(Item))
        end

        it "should create an item" do
          shared.new_id |> Item.get(shared.bot) |> should_not(be_nil())
        end

        it "should update the updated_at for the bot" do
          bot = Repo.get(Bot, shared.bot.id)

          DateTime.compare(bot.updated_at, shared.bot.updated_at)
          |> should(eq :gt)
        end
      end

      context "when an item already exists" do
        before do
          result = Item.put(shared.id, shared.bot, shared.owner, "testing")
          {:ok, result: result}
        end

        it "should return :ok" do
          shared.result |> should(be_ok_result())
        end

        it "should update the item" do
          item = Item.get(shared.id, shared.bot)
          item.stanza |> should(eq "testing")
          item.image |> should(be_false())
        end

        it "should update the updated_at for the bot" do
          bot = Repo.get(Bot, shared.bot.id)

          DateTime.compare(bot.updated_at, shared.bot.updated_at)
          |> should(eq :gt)
        end
      end

      context "when a non-UUID id is supplied" do
        before do
          result = Item.put(Lorem.word(), shared.bot, shared.owner, "testing")
          {:ok, result: result}
        end

        it "should succeed, creating a new UUID ID" do
          shared.result |> should(be_ok_result())
          shared.result |> elem(1) |> Map.get(:id) |> ID.valid?() |> should(be_true())
        end
      end

      context "with invlid input" do
        it "should fail for a non-existant bot" do
          ID.new()
          |> Item.put(Factory.build(:bot), shared.author, Lorem.word())
          |> should(be_error_result())
        end

        it "should fail for a non-existant user" do
          ID.new()
          |> Item.put(shared.bot, Factory.build(:user), Lorem.word())
          |> should(be_error_result())
        end
      end

      it "should set image to true when an image is present" do
        {:ok, item} = Item.put(ID.new(), shared.bot, shared.owner, @image_stanza)

        item.image |> should(be_true())
      end

      describe "permissions" do
        before do
          user = Factory.insert(:user)
          item = Factory.insert(:item, user: user, bot: shared.bot)
          {:ok, user: user, item: item}
        end

        it """
        should refuse to publish an item that already exists
        and is owned by another user
        """ do
          shared.item.id
          |> Item.put(shared.bot, shared.owner, Lorem.paragraph())
          |> should(eq {:error, :permission_denied})
        end

        it """
        should allow publication (update) of an item that already exists
        and is owned by the same user
        """ do
          shared.item.id
          |> Item.put(shared.bot, shared.user, Lorem.paragraph())
          |> should(be_ok_result())
        end
      end
    end

    describe "delete/1" do
      context "when items exists" do
        before do
          result = Item.delete(shared.bot)
          {:ok, result: result}
        end

        it "should return :ok" do
          shared.result |> should(eq :ok)
        end

        it "should remove the items" do
          shared.id |> Item.get(shared.bot) |> should(be_nil())
        end
      end

      it "should return :ok when the bot doesn't exist" do
        Factory.build(:bot)
        |> Item.delete()
        |> should(eq :ok)
      end
    end

    describe "delete/2" do
      context "for an author" do
        before do
          result = Item.delete(shared.bot, shared.author)
          {:ok, result: result}
        end

        it "should return :ok" do
          shared.result |> should(eq :ok)
        end

        it "should remove the item" do
          Item.get(shared.item2.id, shared.bot) |> should(be_nil())
        end
      end

      it "should return :ok when the user doesn't exist" do
        shared.bot
        |> Item.delete(Factory.build(:user))
        |> should(eq :ok)
      end
    end

    describe "delete/3" do
      it "should return {:error, :not_found} when the bot doesn't exist" do
        bot = Factory.build(:bot)

        shared.id
        |> Item.delete(bot, shared.owner)
        |> should(eq {:error, :not_found})
      end

      it "should return {:error, :not_found} when the id doesn't exist" do
        ID.new()
        |> Item.delete(shared.bot, shared.owner)
        |> should(eq {:error, :not_found})
      end

      context "when an item exists" do
        before do
          result = Item.delete(shared.item2.id, shared.bot, shared.author)
          {:ok, result: result}
        end

        it "should return :ok" do
          shared.result |> should(eq :ok)
        end

        it "should remove the item" do
          shared.item2.id |> Item.get(shared.bot) |> should(be_nil())
        end
      end

      context "when an item exists and the bot owner deletes it" do
        before do
          result = Item.delete(shared.item2.id, shared.bot, shared.owner)
          {:ok, result: result}
        end

        it "should return :ok" do
          shared.result |> should(eq :ok)
        end

        it "should remove the item" do
          shared.item2.id |> Item.get(shared.bot) |> should(be_nil())
        end
      end

      context "when an item exists and the bot owner deletes it" do
        before do
          result = Item.delete(shared.id, shared.bot, shared.author)
          {:ok, result: result}
        end

        it "should return {:error, :permission_denied}" do
          shared.result |> should(eq {:error, :permission_denied})
        end

        it "should not remove the item" do
          shared.id |> Item.get(shared.bot) |> should_not(be_nil())
        end
      end
    end
  end

  describe "image detection" do
    describe "has_image/1" do
      it "should detect the presence of an image tag" do
        @image_stanza
        |> Item.has_image()
        |> should(be_true())
      end

      it "should return false if there is no image tag" do
        Lorem.paragraph()
        |> Item.has_image()
        |> should(be_false())

        @no_image_stanza
        |> Item.has_image()
        |> should(be_false())
      end
    end

    describe "get_image/1" do
      it "should get the image value if one is present" do
        @image_stanza
        |> Item.get_image()
        |> should(eq "some_image")
      end

      it "should return nil if no image is present" do
        Lorem.paragraph()
        |> Item.get_image()
        |> should(be_nil())

        @no_image_stanza
        |> Item.get_image()
        |> should(be_nil())
      end
    end
  end
end
