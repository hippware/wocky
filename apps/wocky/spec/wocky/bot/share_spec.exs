defmodule Wocky.Bot.ShareSpec do
  use ESpec, async: true
  use ModelHelpers

  alias Ecto.Adapters.SQL
  alias Wocky.Bot.Share
  alias Wocky.Repo.ID

  describe "validation" do
    let :valid_attrs, do: %{bot_id: ID.new, user_id: ID.new, sharer_id: ID.new}

    it "should pass with valid attributes" do
      %Share{}
      |> Share.changeset(valid_attrs())
      |> should(be_valid())
    end

    it "should fail with missing attributes" do
      %Share{}
      |> Share.changeset(%{})
      |> should(have_errors [:bot_id, :user_id, :sharer_id])
    end

    describe "converting foreign key constraints to errors" do
      let :changeset,
        do: Share.changeset(%Share{}, valid_attrs())

      it do: changeset() |> Repo.insert |> should(be_error_result())

      context "when the bot does not exist" do
        let :new_changeset do
          {:error, changeset} = Repo.insert(changeset())
          changeset
        end

        it "has error" do
          errors = [bot_id: {"does not exist", []},
                    user_id: {"does not exist", []},
                    sharer_id: {"does not exist", []}]
          expect(new_changeset().errors).to have_any(&Enum.member?(errors, &1))
        end
      end
    end
  end

  describe "database operations" do
    before do
      owner = Factory.insert(:user)
      user = Factory.insert(:user)
      bot = Factory.insert(:bot, user: owner)
      share = Factory.insert(:share, user: user, bot: bot)

      {:ok, owner: owner, user: user, bot: bot, share: share}
    end

    describe "exists?/2" do
      it "should return true if the bot is shared to the user" do
        assert Share.exists?(shared.user, shared.bot)
      end

      it "should return false when the user does not exist" do
        user = Factory.build(:user, resource: "testing")
        refute Share.exists?(user, shared.bot)
      end

      it "should return false when the bot does not exist" do
        bot = Factory.build(:bot)
        refute Share.exists?(shared.user, bot)
      end

      it "should return false when the bot is not shared to the user" do
        refute Share.exists?(shared.owner, shared.bot)
      end
    end

    describe "get/2" do
      it "should return the share" do
        Share.get(shared.user, shared.bot) |> should_not(be_nil())
      end

      it "should return nil when the user does not exist" do
        user = Factory.build(:user)
        Share.get(user, shared.bot) |> should(be_nil())
      end

      it "should return nil when the bot does not exist" do
        bot = Factory.build(:bot)
        Share.get(shared.user, bot) |> should(be_nil())
      end

      it "should return nil when the bot is not shared to the user" do
        Share.get(shared.owner, shared.bot) |> should(be_nil())
      end
    end

    describe "put/2" do
      context "when a share does not already exist" do
        before do
          new_user = Factory.insert(:user)
          result = Share.put(new_user, shared.bot, shared.owner)
          {:ok, new_user: new_user, result: result}
        end

        it "should return :ok" do
          shared.result |> should(eq :ok)
        end

        it "should create a subscription" do
          assert Share.exists?(shared.new_user, shared.bot)
        end
      end

      context "when a subscription already exists" do
        before do
          result = Share.put(shared.user, shared.bot, shared.owner)
          {:ok, result: result}
        end

        it "should return :ok" do
          shared.result |> should(eq :ok)
        end
      end
    end

    describe "delete/2" do
      context "when a share exists" do
        before do
          result = Share.delete(shared.user, shared.bot)
          {:ok, result: result}
        end

        it "should return :ok" do
          shared.result |> should(eq :ok)
        end

        it "should remove the subscription" do
          refute Share.exists?(shared.user, shared.bot)
        end
      end

      it "should return :ok when the user doesn't exist" do
        user = Factory.build(:user)
        Share.delete(user, shared.bot)
        |> should(eq :ok)
      end

      it "should return :ok when the bot doesn't exist" do
        bot = Factory.build(:bot)
        Share.delete(shared.user, bot)
        |> should(eq :ok)
      end
    end

    describe "is_shared/2 stored procedure" do
      it "should return true if the bot is shared to the user" do
        assert is_shared_sp(shared.user, shared.bot)
      end

      it "should return false when the user does not exist" do
        user = Factory.build(:user, resource: "testing")
        refute is_shared_sp(user, shared.bot)
      end

      it "should return false when the bot does not exist" do
        bot = Factory.build(:bot)
        refute is_shared_sp(shared.user, bot)
      end

      it "should return false when the bot is not shared to the user" do
        refute is_shared_sp(shared.owner, shared.bot)
      end
    end

  end

  defp is_shared_sp(user, bot) do
    {:ok, u} = Ecto.UUID.dump(user.id)
    {:ok, b} = Ecto.UUID.dump(bot.id)
    Repo
    |> SQL.query!("SELECT is_shared($1, $2)", [u, b])
    |> Map.get(:rows)
    |> hd
    |> hd
  end
end
