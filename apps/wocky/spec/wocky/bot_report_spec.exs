defmodule Wocky.BotReportSpec do
  use ESpec, async: true

  alias Wocky.Repo.Factory
  alias Wocky.BotReport

  before do
    user = Factory.insert(:user)
    bots = Factory.insert_list(5, :bot, %{user: user})
    {:ok, bots: bots}
  end

  let :line_count, do: length(shared.bots) + 1

  describe "generate_report/1" do
    subject do: BotReport.generate_report(1) |> String.strip

    it do: should(be_binary())

    it "should have one line per bot plus one for headers" do
      subject()
      |> String.split("\n")
      |> should(have_length line_count())
    end

    it "should be valid CSV" do
      subject()
      |> String.split("\n")
      |> CSV.decode
      |> Enum.to_list
      |> should(have_all fn {x, _} -> x == :ok end)
    end
  end
end
