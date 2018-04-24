# credo:disable-for-this-file Credo.Check.Refactor.PipeChainStart
defmodule Wocky.Bot.ItemSpec do
  use ESpec, async: true
  use ModelHelpers

  alias Wocky.Bot
  alias Wocky.Bot.Item
  alias Wocky.Repo.ID

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
        bot = Factory.build(:bot)
        Item.get(bot) |> should(be_empty())
      end
    end

    describe "get_count/1" do
      before do
        Factory.insert(:item, bot: shared.bot, image: true)
        Factory.insert(:item, bot: shared.bot, image: false)
        :ok
      end

      it "should return total item count including images" do
        Item.get_count(shared.bot) |> should(eq 4)
      end
    end

    describe "get_images/1" do
      before do
        Factory.insert(:item, bot: shared.bot, user: shared.owner, image: true)
        :ok
      end

      it "should return items that have the image flag set" do
        Item.get_images(shared.bot) |> should(have_count 1)
      end
    end

    describe "get_image_count/1" do
      before do
        Factory.insert(:item, bot: shared.bot, user: shared.owner, image: true)
        :ok
      end

      it "should return items that have the image flag set" do
        Item.get_image_count(shared.bot) |> should(eq 1)
      end
    end

    describe "get/2" do
      it "should return the item" do
        Item.get(shared.bot, shared.id) |> should_not(be_nil())
      end

      it "should return nil when the bot does not exist" do
        bot = Factory.build(:bot)
        Item.get(bot, shared.id) |> should(be_nil())
      end

      it "should return nil when the id does not exist" do
        Item.get(shared.bot, ID.new()) |> should(be_nil())
      end
    end

    describe "put/4" do
      context "when an item does not already exist" do
        before do
          new_id = ID.new()
          result = Item.put(shared.bot, shared.owner, new_id, "testing", true)
          {:ok, new_id: new_id, result: result}
        end

        it "should return :ok" do
          shared.result |> should(eq :ok)
        end

        it "should create an item" do
          Item.get(shared.bot, shared.new_id) |> should_not(be_nil())
        end

        it "should update the updated_at for the bot" do
          bot = Repo.get(Bot, shared.bot.id)

          DateTime.compare(bot.updated_at, shared.bot.updated_at)
          |> should(eq :gt)
        end
      end

      context "when an item already exists" do
        before do
          result = Item.put(shared.bot, shared.owner, shared.id, "testing")
          {:ok, result: result}
        end

        it "should return :ok" do
          shared.result |> should(eq :ok)
        end

        it "should update the item" do
          item = Item.get(shared.bot, shared.id)
          item.stanza |> should(eq "testing")
          item.image |> should(be_false())
        end

        it "should update the updated_at for the bot" do
          bot = Repo.get(Bot, shared.bot.id)

          DateTime.compare(bot.updated_at, shared.bot.updated_at)
          |> should(eq :gt)
        end
      end
    end

    describe "publish/4" do
      before do
        new_id = ID.new()
        result = Item.publish(shared.bot, shared.owner, new_id, "testing", true)
        {:ok, new_id: new_id, result: result}
      end

      it "should return the item" do
        {:ok, item} = shared.result
        item.id |> should(eq shared.new_id)
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
          Item.get(shared.bot, shared.id) |> should(be_nil())
        end
      end

      it "should return :ok when the bot doesn't exist" do
        bot = Factory.build(:bot)

        Item.delete(bot)
        |> should(eq :ok)
      end
    end

    describe "delete/2" do
      context "when an item exists" do
        before do
          result = Item.delete(shared.bot, shared.id)
          {:ok, result: result}
        end

        it "should return :ok" do
          shared.result |> should(eq :ok)
        end

        it "should remove the item" do
          Item.get(shared.bot, shared.id) |> should(be_nil())
        end
      end

      context "for an author" do
        before do
          result = Item.delete(shared.bot, shared.author)
          {:ok, result: result}
        end

        it "should return :ok" do
          shared.result |> should(eq :ok)
        end

        it "should remove the item" do
          Item.get(shared.bot, shared.item2.id) |> should(be_nil())
        end
      end

      it "should return :ok when the bot doesn't exist" do
        bot = Factory.build(:bot)

        Item.delete(bot, shared.id)
        |> should(eq :ok)
      end

      it "should return :ok when the id doesn't exist" do
        Item.delete(shared.bot, ID.new())
        |> should(eq :ok)
      end

      it "should return :ok when the user doesn't exist" do
        Item.delete(shared.bot, Factory.build(:user))
        |> should(eq :ok)
      end
    end
  end
end
