defmodule Wocky.Repo.MaintenanceTasks do
  @moduledoc "Functions that keep the database nice and clean."

  import Ecto.Query

  alias Wocky.Bot
  alias Wocky.Repo
  alias Wocky.Repo.Timestamp
  alias Wocky.Token
  alias Wocky.TrafficLog
  alias Wocky.TROSMetadata

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
    expire_date = Timestamp.shift(hours: -1)

    {deleted, nil} =
      TROSMetadata
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
    {:ok, 0}
  end

  def clean_bot_image_links do
    {:ok, 0}
  end

  def clean_user_avatar_links do
    {:ok, 0}
  end
end
