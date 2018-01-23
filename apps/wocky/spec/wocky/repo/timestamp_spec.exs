defmodule Wocky.Repo.TimestampSpec do
  use ESpec, async: true

  alias Timex.Duration
  alias Wocky.Repo.Timestamp

  @iso8601_regex Timestamp.regex()

  describe "expired?/1" do
    it do
      DateTime.utc_now()
      |> Timex.subtract(Duration.from_seconds(100))
      |> Timestamp.expired?()
      |> assert()
    end

    it do
      DateTime.utc_now()
      |> Timex.add(Duration.from_seconds(100))
      |> Timestamp.expired?()
      |> refute()
    end
  end

  describe "from_string/1" do
    before do
      now = DateTime.utc_now()
      result = now |> Timestamp.to_string() |> Timestamp.from_string()
      {:ok, now: now, result: result}
    end

    it "should return a success result" do
      shared.result |> should(be_ok_result())
    end

    it "should return the original timestamp" do
      {:ok, dt} = shared.result
      dt |> should(eq shared.now)
    end

    it "should return an error with bad data" do
      "bogus" |> Timestamp.from_string() |> should(be_error_result())
    end
  end

  describe "to_string/1" do
    it do
      DateTime.utc_now()
      |> Timestamp.to_string()
      |> should(match(@iso8601_regex))
    end
  end

  describe "less_than_eq?/2" do
    before do
      ts1 = DateTime.utc_now()
      ts2 = DateTime.utc_now()
      {:ok, ts1: ts1, ts2: ts2}
    end

    it do: assert(Timestamp.less_than_eq?(shared.ts1, shared.ts2))
    it do: assert(Timestamp.less_than_eq?(shared.ts1, shared.ts1))
    it do: refute(Timestamp.less_than_eq?(shared.ts2, shared.ts1))
  end

  describe "shift/1" do
    it do:
         Timestamp.shift(days: -1)
         |> Timestamp.from_string!()
         |> DateTime.compare(DateTime.utc_now())
         |> should(eq :lt)

    it do:
         Timestamp.shift(days: 1)
         |> Timestamp.from_string!()
         |> DateTime.compare(DateTime.utc_now())
         |> should(eq :gt)
  end
end
