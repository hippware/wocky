defmodule Wocky.Repo.TimestampTest do
  use ExUnit.Case, async: true

  alias Timex.Duration
  alias Wocky.Repo.Timestamp

  @iso8601_regex Timestamp.regex()

  test "expired?/1" do
    assert DateTime.utc_now()
           |> Timex.subtract(Duration.from_seconds(100))
           |> Timestamp.expired?()

    refute DateTime.utc_now()
           |> Timex.add(Duration.from_seconds(100))
           |> Timestamp.expired?()
  end

  describe "from_string/1" do
    test "should return the original timestamp" do
      now = DateTime.utc_now()
      result = now |> Timestamp.to_string() |> Timestamp.from_string()

      assert {:ok, ^now} = result
    end

    test "should return an error with bad data" do
      assert {:error, _} = Timestamp.from_string("bogus")
    end
  end

  test "to_string/1" do
    assert Timestamp.to_string(DateTime.utc_now()) =~ @iso8601_regex
  end

  test "less_than_eq?/2" do
    ts1 = DateTime.utc_now()
    ts2 = DateTime.utc_now()

    assert Timestamp.less_than_eq?(ts1, ts2)
    assert Timestamp.less_than_eq?(ts1, ts1)
    refute Timestamp.less_than_eq?(ts2, ts1)
  end

  test "shift/1" do
    now = DateTime.utc_now()

    assert DateTime.compare(Timestamp.shift(days: -1), now) == :lt
    assert DateTime.compare(Timestamp.shift(days: 1), now) == :gt
  end
end
