defmodule Wocky.POI.ItemTest do
  use Wocky.DataCase, async: true

  alias Wocky.POI.Item
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
end
