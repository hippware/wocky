defmodule Wocky.Audit.LocationLogTest do
  use ExUnit.Case, async: true

  import Wocky.DataCase

  alias Wocky.Audit.LocationLog
  alias Wocky.Repo.Factory

  setup do
    user = Factory.build(:user)

    {:ok, user: user}
  end

  describe "changeset/2 validations" do
    setup %{user: user} do
      {:ok, loc: %LocationLog{user: user}}
    end

    test "should pass with valid attributes", %{loc: loc} do
      data = %{
        device: "testing",
        lat: 1.0,
        lon: 1.0,
        accuracy: 10,
        captured_at: DateTime.utc_now()
      }

      changeset = LocationLog.changeset(loc, data)
      assert changeset.valid?
    end

    test "should fail if fields are missing", %{loc: loc} do
      changeset = LocationLog.changeset(loc, %{})
      refute changeset.valid?

      for a <- [:device, :lat, :lon, :accuracy, :captured_at] do
        assert "can't be blank" in errors_on(changeset)[a]
      end
    end

    test "should fail if the accuracy is negative", %{loc: loc} do
      data = %{device: "testing", lat: 1.0, lon: 1.0, accuracy: -1}
      changeset = LocationLog.changeset(loc, data)
      assert errors_on(changeset)[:accuracy]
    end
  end
end
