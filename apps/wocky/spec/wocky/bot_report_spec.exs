defmodule Wocky.BotReportSpec do
  use ESpec, async: true

  alias Wocky.Repo.Factory
  alias Wocky.BotReport

  before do
    user = Factory.insert(:user)
    for _ <- 1..5, do: Factory.insert(:bot, %{user: user})
  end

  describe "generate_bot_report/1 and encode_as_csv/1" do
    subject do
      1
      |> BotReport.generate_bot_report
      |> BotReport.encode_as_csv
    end

    it do: should(be_binary())
    it "should have one line per bot plus one for headers" do
      subject()
      |> String.split("\n")
      |> should(have_length 7) # String.split creates an extra "" at the end
    end
  end
end
