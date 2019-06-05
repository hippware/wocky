defmodule Wocky.Location.UserLocationTest do
  use Wocky.DataCase

  alias Wocky.Location.UserLocation
  alias Wocky.Repo.Factory

  setup do
    user = Factory.insert(:user)

    {:ok, user: user}
  end

  describe "changeset/2 validations" do
    setup %{user: user} do
      {:ok, loc: %UserLocation{user: user}}
    end

    test "should pass with valid attributes", %{loc: loc} do
      data = %{
        device: "testing",
        lat: 1.0,
        lon: 1.0,
        accuracy: 10,
        captured_at: DateTime.utc_now()
      }

      changeset = UserLocation.changeset(loc, data)
      assert changeset.valid?
    end

    test "should fail if fields are missing", %{loc: loc} do
      changeset = UserLocation.changeset(loc, %{})
      refute changeset.valid?

      for a <- [:device, :lat, :lon, :accuracy, :captured_at] do
        assert "can't be blank" in errors_on(changeset)[a]
      end
    end

    test "should fail if the accuracy is negative", %{loc: loc} do
      data = %{device: "testing", lat: 1.0, lon: 1.0, accuracy: -1}
      changeset = UserLocation.changeset(loc, data)
      assert errors_on(changeset)[:accuracy]
    end
  end
end
