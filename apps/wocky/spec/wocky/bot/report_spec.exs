defmodule Wocky.Bot.ReportSpec do
  use ESpec, async: true

  alias Wocky.Bot
  alias Wocky.Bot.Report
  alias Wocky.Repo
  alias Wocky.Repo.Factory

  before do
    Repo.delete_all(Bot)
    user = Factory.insert(:user)
    bots = Factory.insert_list(5, :bot, %{user: user})
    pending = Factory.insert(:bot, %{user: user, pending: true})

    {:ok,
     bots: bots,
     bot_ids: Enum.sort(Enum.map(bots, &Map.get(&1, :id))),
     pending: pending}
  end

  let :line_count, do: length(shared.bots) + 1

  describe "generate_report/1" do
    subject do: Report.generate_report(1) |> String.trim()

    it do: should(be_binary())

    it "should have one line per bot plus one for headers" do
      subject()
      |> String.split("\n")
      |> should(have_length line_count())
    end

    it "should be valid CSV" do
      subject()
      |> String.split("\n")
      |> CSV.decode()
      |> Enum.to_list()
      |> should(have_all(fn {x, _} -> x == :ok end))
    end

    it "should have all the bots except pending ones" do
      subject()
      |> String.split("\n")
      |> CSV.decode!()
      |> Enum.to_list()
      |> tl
      |> Enum.map(&hd/1)
      |> Enum.sort()
      |> should(eq shared.bot_ids)
    end
  end
end
