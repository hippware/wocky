defmodule Wocky.BotReport do
  @moduledoc "Generate a report of active bots and post to Slack"

  import Ecto.Query

  alias Slackex.Files
  alias Timex.Duration
  alias Wocky.Bot
  alias Wocky.Bot.Item
  alias Wocky.Repo

  @header ~w(
    ID Title Owner Created Updated Address Latitude Longitude
    Visibility Subscribers ImageItems Description
  )

  @spec run(binary, non_neg_integer) :: binary
  def run(channel, days) do
    if Confex.get(:wocky, :enable_bot_report) do
      post_bot_report(channel, days)
    end
  end

  def post_bot_report(channel, days) do
    # FIXME This application should have no visibility into wocky_xmpp,
    # but there wasn't a better way to handle this situation.
    servers = Application.get_env(:wocky_xmpp, :servers, ["localhost"])
    server = hd(servers)
    report =
      days
      |> generate_bot_report()
      |> encode_as_csv()
    Files.upload(%{content: report,
                   filename: "weekly_bot_report_#{server}.csv",
                   title: "Weekly Bot Report for #{server}",
                   filetype: "csv",
                   channels: channel})
  end

  def encode_as_csv(report) do
    report
    |> CSV.encode
    |> Enum.join
  end

  def generate_bot_report(days) do
    aft =
      Timex.now
      |> Timex.subtract(Duration.from_days(days))
      |> Timex.to_naive_datetime

    report =
      Bot
      |> where([b], b.created_at > ^aft)
      |> Repo.all
      |> Enum.map(&report_bot(&1))

    [@header | report]
  end

  defp report_bot(%Bot{} = bot) do
    %{handle: handle} = Bot.owner(bot)
    [
      bot.id,
      bot.title,
      handle,
      bot.created_at,
      bot.updated_at,
      bot.address,
      bot.lat,
      bot.lon,
      vis_string(bot.public),
      Bot.subscriber_count(bot),
      Item.get_image_count(bot),
      bot.description
    ]
    |> Enum.map(&to_string/1)
  end

  defp vis_string(true), do: "public"
  defp vis_string(_), do: "private"
end
