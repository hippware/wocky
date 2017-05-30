defmodule Wocky.Repo.TimestampSpec do
  use ESpec, async: true

  alias Wocky.Repo.Timestamp

  @iso8601_rx ~r/\d\d\d\d-\d\d-\d\dT\d\d:\d\d:\d\dZ/

  describe "now/1" do
    subject do: Timestamp.now

    it do: should(be_integer())
  end

  describe "expired?/1" do
    it do: assert Timestamp.expired?(Timestamp.now - 100)
    it do: refute Timestamp.expired?(Timestamp.now + 100)
  end

  describe "to_string/1" do
    it do: Timestamp.now |> Timestamp.to_string |> should(match @iso8601_rx)

    it do
      Timestamp.now
      |> Timex.from_unix
      |> Timestamp.to_string
      |> should(match @iso8601_rx)
    end
  end

  describe "less_than_eq?/2" do
    before do
      ts1 = DateTime.utc_now
      ts2 = DateTime.utc_now
      {:ok, ts1: ts1, ts2: ts2}
    end

    it do: assert Timestamp.less_than_eq?(shared.ts1, shared.ts2)
    it do: assert Timestamp.less_than_eq?(shared.ts1, shared.ts1)
    it do: refute Timestamp.less_than_eq?(shared.ts2, shared.ts1)
  end
end
