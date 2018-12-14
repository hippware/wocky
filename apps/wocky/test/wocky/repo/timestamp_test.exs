defmodule Wocky.Repo.TimestampTest do
  use ExUnit.Case, async: true

  alias Wocky.Repo.Timestamp

  @iso8601_regex ~r|\d\d\d\d-\d\d-\d\dT\d\d:\d\d:\d\d\.\d?\d?\d?\d?\d?\d?Z|

  describe "from_string/1" do
    test "should return the original timestamp" do
      now = DateTime.utc_now()
      result = now |> Timestamp.to_string!() |> Timestamp.from_string!()

      assert result == now
    end
  end

  test "to_string/1" do
    assert Timestamp.to_string!(DateTime.utc_now()) =~ @iso8601_regex
  end

  test "shift/1" do
    now = DateTime.utc_now()

    assert DateTime.compare(Timestamp.shift(days: -1), now) == :lt
    assert DateTime.compare(Timestamp.shift(days: 1), now) == :gt
  end
end
