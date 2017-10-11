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

  @spec run :: nil | binary
  def run do
    {:ok, _} = Application.ensure_all_started(:wocky)
    if Confex.get(:wocky, :enable_bot_report) do
      days = Confex.get(:wocky, :bot_report_days)
      report = generate_report(days)

      server = Confex.get(:wocky, :wocky_host)
      channel = Confex.get(:wocky, :bot_report_channel)
      Files.upload(%{content: report,
                     filename: "weekly_bot_report_#{server}.csv",
                     title: "Weekly Bot Report for #{server}",
                     filetype: "csv",
                     channels: channel})
    end
  end

  @spec generate_report(non_neg_integer) :: binary
  def generate_report(days) do
    {:ok, csv} =
      Repo.transaction fn ->
        days
        |> since()
        |> get_bot_data()
        |> add_header()
        |> Enum.join
      end

    csv
  end

  defp add_header(data) do
    [@header]
    |> CSV.encode
    |> Stream.concat(data)
  end

  defp since(days) do
    Timex.now
    |> Timex.subtract(Duration.from_days(days))
    |> Timex.to_naive_datetime
  end

  defp get_bot_data(since) do
    Bot
    |> where([b], b.created_at > ^since and not b.pending)
    |> Repo.stream
    |> Stream.map(&format_bot/1)
    |> CSV.encode
  end

  defp format_bot(%Bot{} = bot) do
    [
      bot.id,
      bot.title,
      owner_handle(bot),
      bot.created_at,
      bot.updated_at,
      bot.address,
      Bot.lat(bot),
      Bot.lon(bot),
      vis_string(bot.public),
      Bot.subscriber_count(bot),
      Item.get_image_count(bot),
      bot.description
    ]
    |> Enum.map(&to_string/1)
  end

  defp owner_handle(bot), do: Bot.owner(bot).handle

  defp vis_string(true), do: "public"
  defp vis_string(_), do: "private"
end
