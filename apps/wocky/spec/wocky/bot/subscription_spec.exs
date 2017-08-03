defmodule Wocky.Bot.SubscriptionSpec do
  use ESpec, async: true
  use ModelHelpers

  alias Ecto.Adapters.SQL
  alias Wocky.Bot.Subscription
  alias Wocky.Repo.ID

  describe "validation" do
    let :valid_attrs, do: %{bot_id: ID.new, user_id: ID.new}

    it "should pass with valid attributes" do
      %Subscription{}
      |> Subscription.changeset(valid_attrs())
      |> should(be_valid())
    end

    it "should fail with missing attributes" do
      %Subscription{}
      |> Subscription.changeset(%{})
      |> should(have_errors [:bot_id, :user_id])
    end

    describe "converting foreign key constraints to errors" do
      let :changeset,
        do: Subscription.changeset(%Subscription{}, valid_attrs())

      it do: changeset() |> Repo.insert |> should(be_error_result())

      context "when the user does not exist" do
        let :new_changeset do
          {:error, changeset} = Repo.insert(changeset())
          changeset
        end

        it "has error" do
          error = {:user_id, {"does not exist", []}}
          expect(new_changeset().errors).to have(error)
        end
      end
    end
  end

  describe "database operations" do
    before do
      owner = Factory.insert(:user)
      user = Factory.insert(:user)
      bot = Factory.insert(:bot, user: owner)
      sub = Factory.insert(:subscription, user: user, bot: bot)

      {:ok, owner: owner, user: user, bot: bot, sub: sub}
    end

    describe "exists?/2" do
      it "should return true if the user is subscribed to the bot" do
        assert Subscription.exists?(shared.user, shared.bot)
      end

      it "should return false when the user does not exist" do
        user = Factory.build(:user, resource: "testing")
        refute Subscription.exists?(user, shared.bot)
      end

      it "should return false when the bot does not exist" do
        bot = Factory.build(:bot)
        refute Subscription.exists?(shared.user, bot)
      end

      it "should return false when the user is not subscribed to the bot" do
        refute Subscription.exists?(shared.owner, shared.bot)
      end
    end

    describe "get/2" do
      it "should return the subscription" do
        Subscription.get(shared.user, shared.bot) |> should_not(be_nil())
      end

      it "should return nil when the user does not exist" do
        user = Factory.build(:user)
        Subscription.get(user, shared.bot) |> should(be_nil())
      end

      it "should return nil when the bot does not exist" do
        bot = Factory.build(:bot)
        Subscription.get(shared.user, bot) |> should(be_nil())
      end

      it "should return nil when the user is not subscribed to the bot" do
        Subscription.get(shared.owner, shared.bot) |> should(be_nil())
      end
    end

    describe "put/2" do
      context "when a subscription does not already exist" do
        before do
          new_user = Factory.insert(:user)
          result = Subscription.put(new_user, shared.bot)
          {:ok, new_user: new_user, result: result}
        end

        it "should return :ok" do
          shared.result |> should(eq :ok)
        end

        it "should create a subscription" do
          assert Subscription.exists?(shared.new_user, shared.bot)
        end
      end

      context "when a subscription already exists" do
        before do
          result = Subscription.put(shared.user, shared.bot)
          {:ok, result: result}
        end

        it "should return :ok" do
          shared.result |> should(eq :ok)
        end
      end
    end

    describe "delete/2" do
      context "when a subscription exists" do
        before do
          result = Subscription.delete(shared.user, shared.bot)
          {:ok, result: result}
        end

        it "should return :ok" do
          shared.result |> should(eq :ok)
        end

        it "should remove the subscription" do
          refute Subscription.exists?(shared.user, shared.bot)
        end
      end

      it "should return :ok when the user doesn't exist" do
        user = Factory.build(:user)
        Subscription.delete(user, shared.bot)
        |> should(eq :ok)
      end

      it "should return :ok when the bot doesn't exist" do
        bot = Factory.build(:bot)
        Subscription.delete(shared.user, bot)
        |> should(eq :ok)
      end
    end

    describe "is_subscribed/2 stored procedure" do
      it "should return true if the user is subscribed to the bot" do
        assert is_subscribed_sp(shared.user, shared.bot)
      end

      it "should return false when the user does not exist" do
        user = Factory.build(:user, resource: "testing")
        refute is_subscribed_sp(user, shared.bot)
      end

      it "should return false when the bot does not exist" do
        bot = Factory.build(:bot)
        refute is_subscribed_sp(shared.user, bot)
      end

      it "should return false when the user is not subscribed to the bot" do
        refute is_subscribed_sp(shared.owner, shared.bot)
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
end
