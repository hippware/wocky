defmodule Wocky.Bot.Report do
  @moduledoc "Generate a report of active bots and post to Slack"

  import Ecto.Query

  alias Slack.File
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

    if Confex.get_env(:wocky, :enable_bot_report) do
      :wocky
      |> Confex.get_env(:bot_report_days)
      |> generate_report()
      |> publish_report()
    end
  end

  @spec generate_report(non_neg_integer) :: binary
  def generate_report(days) do
    now = Timex.now()
    generate_report(Timex.shift(now, days: -days), now)
  end

  @spec generate_report(DateTime.t(), DateTime.t()) :: binary
  def generate_report(first, last) do
    {:ok, csv} =
      Repo.transaction(fn ->
        first
        |> get_bot_data(last)
        |> add_header()
        |> Enum.join()
      end)

    csv
  end

  @spec publish_report(binary) :: any
  def publish_report(report) do
    server = Confex.get_env(:wocky, :wocky_host)
    channel = Confex.get_env(:wocky, :bot_report_channel)

    :wocky
    |> Confex.get_env(:slack_token)
    |> Slack.client()
    |> File.upload(
      content: report,
      filename: "weekly_bot_report_#{server}.csv",
      title: "Weekly Bot Report for #{server}",
      filetype: "csv",
      channels: channel
    )
  end

  defp add_header(data) do
    [@header]
    |> CSV.encode()
    |> Stream.concat(data)
  end

  defp get_bot_data(first, last) do
    Bot
    |> where([b], b.created_at > ^first and b.created_at <= ^last)
    |> where([b], not b.pending)
    |> Repo.stream()
    |> Stream.map(&format_bot/1)
    |> CSV.encode()
  end

  defp format_bot(%Bot{} = bot) do
    [
      bot.id,
      word_count(bot.title),
      owner_handle(bot),
      bot.created_at,
      bot.updated_at,
      bot.address,
      Bot.lat(bot),
      Bot.lon(bot),
      vis_string(bot.public),
      Bot.subscriber_count(bot),
      Item.get_image_count(bot),
      word_count(bot.description)
    ]
    |> Enum.map(&to_string/1)
  end

  defp owner_handle(bot), do: Bot.owner(bot).handle

  defp vis_string(true), do: "public"
  defp vis_string(_), do: "private"

  defp word_count(words), do: words |> String.split() |> Enum.count()
end
