defmodule Wocky.Bot.TempSubscriptionSpec do
  use ESpec, async: true
  use ModelHelpers

  alias Wocky.Bot.TempSubscription
  alias Wocky.Repo.ID
  alias Wocky.User

  describe "validation" do
    let :valid_attrs,
      do: %{bot_id: ID.new, user_id: ID.new, resource: "test", node: "foo"}

    it "should pass with valid attributes" do
      %TempSubscription{}
      |> TempSubscription.changeset(valid_attrs())
      |> should(be_valid())
    end

    it "should fail with missing attributes" do
      %TempSubscription{}
      |> TempSubscription.changeset(%{})
      |> should(have_errors [:bot_id, :user_id, :resource, :node])
    end

    describe "converting foreign key constraints to errors" do
      let :changeset,
        do: TempSubscription.changeset(%TempSubscription{}, valid_attrs())

      it do: changeset() |> Repo.insert |> should(be_error_result())

      context "when the bot does not exist" do
        let :new_changeset do
          {:error, changeset} = Repo.insert(changeset())
          changeset
        end

        it "has error" do
          error = {:bot_id, {"does not exist", []}}
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
      temp_sub = Factory.insert(:temp_subscription, user: user, bot: bot)

      user = %User{user | resource: temp_sub.resource}

      {:ok, owner: owner, user: user, bot: bot, sub: temp_sub}
    end

    describe "exists?/2" do
      it "should return true if the user is subscribed to the bot" do
        assert TempSubscription.exists?(shared.user, shared.bot)
      end

      it "should return false when the user does not exist" do
        user = Factory.build(:user, resource: "testing")
        refute TempSubscription.exists?(user, shared.bot)
      end

      it "should return false when the bot does not exist" do
        bot = Factory.build(:bot)
        refute TempSubscription.exists?(shared.user, bot)
      end

      it "should return false when the user is not subscribed to the bot" do
        user = %User{shared.owner | resource: shared.sub.resource}
        refute TempSubscription.exists?(user, shared.bot)
      end

      it "should return false when the user resource does not match" do
        user = %User{shared.user | resource: "testing"}
        refute TempSubscription.exists?(user, shared.bot)
      end
    end

    describe "get/2" do
      it "should return the subscription" do
        temp_sub = TempSubscription.get(shared.user, shared.bot)
        temp_sub.resource |> should(eq shared.sub.resource)
        temp_sub.node |> should(eq shared.sub.node)
      end

      it "should return nil when the user does not exist" do
        user = Factory.build(:user, resource: "testing")
        TempSubscription.get(user, shared.bot)
        |> should(be_nil())
      end

      it "should return nil when the bot does not exist" do
        bot = Factory.build(:bot)
        TempSubscription.get(shared.user, bot)
        |> should(be_nil())
      end

      it "should return nil when the user is not subscribed to the bot" do
        user = %User{shared.owner | resource: shared.sub.resource}
        TempSubscription.get(user, shared.bot)
        |> should(be_nil())
      end

      it "should return nil when the user resource does not match" do
        user = %User{shared.user | resource: "testing"}
        TempSubscription.get(user, shared.bot)
        |> should(be_nil())
      end
    end

    describe "put/3" do
      context "when a subscription does not already exist" do
        before do
          new_user = Factory.insert(:user, resource: "testing")
          result = TempSubscription.put(new_user, shared.bot, "testing")
          {:ok, new_user: new_user, result: result}
        end

        it "should return :ok" do
          shared.result |> should(eq :ok)
        end

        it "should create a subscription" do
          temp_sub = TempSubscription.get(shared.new_user, shared.bot)
          temp_sub.resource |> should(eq "testing")
          temp_sub.node |> should(eq "testing")
        end
      end

      context "when a subscription already exists" do
        before do
          result = TempSubscription.put(shared.user, shared.bot, "testing")
          {:ok, result: result}
        end

        it "should return :ok" do
          shared.result |> should(eq :ok)
        end

        it "should update the subscription" do
          temp_sub = TempSubscription.get(shared.user, shared.bot)
          temp_sub.node |> should(eq "testing")
        end
      end
    end

    describe "delete/2" do
      context "when a subscription exists" do
        before do
          result = TempSubscription.delete(shared.user, shared.bot)
          {:ok, result: result}
        end

        it "should return :ok" do
          shared.result |> should(eq :ok)
        end

        it "should remove the subscription" do
          refute TempSubscription.exists?(shared.user, shared.bot)
        end
      end

      it "should return :ok when the user doesn't exist" do
        user = Factory.build(:user)
        TempSubscription.delete(user, shared.bot)
        |> should(eq :ok)
      end

      it "should return :ok when the bot doesn't exist" do
        bot = Factory.build(:bot)
        TempSubscription.delete(shared.user, bot)
        |> should(eq :ok)
      end
    end

    describe "delete/1" do
      context "when passed a user" do
        context "when a subscription exists" do
          before do
            result = TempSubscription.delete(shared.user)
            {:ok, result: result}
          end

          it "should return :ok" do
            shared.result |> should(eq :ok)
          end

          it "should remove the subscription" do
            refute TempSubscription.exists?(shared.user, shared.bot)
          end
        end

        it "should return :ok when the user doesn't exist" do
          user = Factory.build(:user)
          TempSubscription.delete(user)
          |> should(eq :ok)
        end
      end

      context "when passed a node" do
        context "when a subscription exists" do
          before do
            result = TempSubscription.delete(shared.sub.node)
            {:ok, result: result}
          end

          it "should return :ok" do
            shared.result |> should(eq :ok)
          end

          it "should remove the subscription" do
            refute TempSubscription.exists?(shared.user, shared.bot)
          end
        end

        it "should return :ok when the node doesn't have any subscriptions" do
          TempSubscription.delete("notanode")
          |> should(eq :ok)
        end
      end
    end
  end
end
