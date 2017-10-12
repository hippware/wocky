defmodule Wocky.Repo.MaintenanceTasks do
  @moduledoc "Functions that keep the database nice and clean."

  import Ecto.Query
  import SweetXml

  alias Wocky.Bot
  alias Wocky.Bot.Item
  alias Wocky.Repo
  alias Wocky.Repo.Timestamp
  alias Wocky.Token
  alias Wocky.TrafficLog
  alias Wocky.TROS
  alias Wocky.TROS.Metadata
  alias Wocky.User

  require Logger

  def clean_all do
    {:ok, _} = Application.ensure_all_started(:wocky)

    _ = clean_pending_bots()
    _ = clean_traffic_logs()
    _ = clean_expired_auth_tokens()
    _ = clean_pending_tros_files()
    _ = clean_dead_tros_links()

    :ok
  end

  def clean_pending_bots do
    expire_date = Timestamp.shift(days: -1)

    {deleted, nil} =
      Bot
      |> where([b], b.pending == true)
      |> where([b], b.created_at <= ^expire_date)
      |> Repo.delete_all

    Logger.info("Deleted #{deleted} pending bots created before #{expire_date}")

    {:ok, deleted}
  end

  def clean_traffic_logs do
    expire_date = Timestamp.shift(months: -1)

    {deleted, nil} =
      TrafficLog
      |> where([t], t.created_at <= ^expire_date)
      |> Repo.delete_all

    Logger.info("Deleted #{deleted} traffic logs created before #{expire_date}")

    {:ok, deleted}
  end

  def clean_expired_auth_tokens do
    {deleted, nil} =
      Token
      |> where([t], t.expires_at < ^DateTime.utc_now)
      |> Repo.delete_all

    Logger.info("Deleted #{deleted} expired authentication tokens")

    {:ok, deleted}
  end

  def clean_pending_tros_files do
    expire_date = Timestamp.shift(weeks: -1)

    {deleted, nil} =
      Metadata
      |> where([t], t.ready == false)
      |> where([t], t.created_at < ^expire_date)
      |> Repo.delete_all

    Logger.info(
      "Deleted #{deleted} pending files created before #{expire_date}")

    {:ok, deleted}
  end

  def clean_dead_tros_links do
    {:ok, d1} = clean_bot_item_image_links()
    {:ok, d2} = clean_bot_image_links()
    {:ok, d3} = clean_user_avatar_links()

    {:ok, d1 + d2 + d3}
  end

  def clean_bot_item_image_links do
    Repo.transaction(fn ->
      Item
      |> where(image: true)
      |> Repo.stream
      |> Stream.map(&extract_item_image/1)
      |> Stream.filter(fn {_, image} -> image_missing?(image) end)
      |> Stream.each(&purge_missing_item_image/1)
      |> Enum.count
    end)
  end

  defp extract_item_image(item) do
    image = xpath(item.stanza, ~x"/entry/image/text()"S)
    {item, image}
  end

  defp image_missing?(""), do: false
  defp image_missing?(image_url) do
    case TROS.parse_url(image_url) do
      {:ok, {_, file_id}} -> Metadata.get(file_id) == nil
      {:error, _} -> true
    end
  end

  defp purge_missing_item_image({item, image}) do
    content = xpath(item.stanza, ~x"/entry/content/text()"S)
    if String.length(content) > 0 do
      image_tag = "<image>#{image}</image>"
      new_stanza = String.replace(item.stanza, image_tag, "")

      item
      |> Item.changeset(%{stanza: new_stanza, image: false})
      |> Repo.update!
    else
      Repo.delete!(item)
    end
  end

  def clean_bot_image_links do
    Repo.transaction(fn ->
      Bot
      |> where([b], not is_nil(b.image))
      |> where([b], b.image != "")
      |> Repo.stream
      |> Stream.filter(&image_missing?(&1.image))
      |> Stream.each(&purge_missing_bot_image/1)
      |> Enum.count
    end)
  end

  defp purge_missing_bot_image(bot) do
    bot
    |> Bot.changeset(%{image: nil})
    |> Repo.update!
  end

  def clean_user_avatar_links do
    Repo.transaction(fn ->
      User
      |> where([u], not is_nil(u.avatar))
      |> where([u], u.avatar != "")
      |> Repo.stream
      |> Stream.filter(&image_missing?(&1.avatar))
      |> Stream.each(&purge_missing_user_image/1)
      |> Enum.count
    end)
  end

  defp purge_missing_user_image(user) do
    user
    |> User.changeset(%{avatar: nil})
    |> Repo.update!
  end
end
