defmodule Wocky.POI.BotTest do
  use ExUnit.Case, async: true

  import Wocky.DataCase

  alias Wocky.GeoUtils
  alias Wocky.POI.Bot
  alias Wocky.Repo.ID

  describe "validations" do
    setup do
      {:ok,
       attrs: %{
         id: ID.new(),
         user_id: ID.new(),
         title: "test bot",
         location: GeoUtils.point(5.0, 5.0)
       }}
    end

    test "should pass with valid attributes", %{attrs: attrs} do
      assert Bot.changeset(%Bot{}, attrs).valid?
    end

    test "should set pending to 'false'", %{attrs: attrs} do
      changeset = Bot.changeset(%Bot{}, attrs)
      refute changeset.changes.pending
    end

    test "should fail with missing fields", %{attrs: attrs} do
      changeset = Bot.changeset(%Bot{}, %{})

      refute changeset.valid?

      errors = Map.keys(errors_on(changeset))

      for {attr, _} <- attrs do
        assert Enum.member?(errors, attr)
      end
    end

    test "should fail with negative radius", %{attrs: attrs} do
      changeset = Bot.changeset(%Bot{}, Map.put(attrs, :radius, -1))
      refute changeset.valid?
      assert errors_on(changeset).radius
    end

    test "should fail with a nil description", %{attrs: attrs} do
      changeset = Bot.changeset(%Bot{}, Map.put(attrs, :description, nil))
      refute changeset.valid?
      assert errors_on(changeset).description
    end
  end
end
