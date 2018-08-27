defmodule Wocky.User.LocationTest do
  use Wocky.DataCase

  alias Wocky.Repo.Factory
  alias Wocky.User.Location

  setup do
    user = Factory.insert(:user)

    {:ok, user: user}
  end

  describe "changeset/2 validations" do
    setup %{user: user} do
      {:ok, loc: %Location{user: user}}
    end

    test "should pass with valid attributes", %{loc: loc} do
      data = %{
        resource: "testing",
        lat: 1.0,
        lon: 1.0,
        accuracy: 10,
        captured_at: DateTime.utc_now()
      }

      changeset = Location.changeset(loc, data)
      assert changeset.valid?
    end

    test "should fail if fields are missing", %{loc: loc} do
      changeset = Location.changeset(loc, %{})
      refute changeset.valid?

      for a <- [:resource, :lat, :lon, :accuracy, :captured_at] do
        assert "can't be blank" in errors_on(changeset)[a]
      end
    end

    test "should fail if the accuracy is negative", %{loc: loc} do
      data = %{resource: "testing", lat: 1.0, lon: 1.0, accuracy: -1}
      changeset = Location.changeset(loc, data)
      assert errors_on(changeset)[:accuracy]
    end
  end
end
