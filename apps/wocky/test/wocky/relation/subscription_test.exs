defmodule Wocky.Relation.SubscriptionTest do
  use Wocky.DataCase, async: true

  alias Wocky.Relation.Subscription
  alias Wocky.Repo.ID

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
end
