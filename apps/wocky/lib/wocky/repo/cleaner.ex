defmodule Wocky.Repo.Cleaner do
  @moduledoc "Functions that keep the database nice and clean."

  import Ecto.Query
  import SweetXml

  alias Wocky.Bot
  alias Wocky.Bot.Item
  alias Wocky.Push.Log, as: PushLog
  alias Wocky.Push.Token, as: PushToken
  alias Wocky.Repo
  alias Wocky.Repo.Timestamp
  alias Wocky.TrafficLog
  alias Wocky.TROS
  alias Wocky.TROS.Metadata
  alias Wocky.User
  alias Wocky.User.InviteCode

  require Logger

  def clean_all do
    {:ok, d1} = clean_traffic_logs()
    {:ok, d2} = clean_notification_logs()
    {:ok, d3} = clean_pending_users()
    {:ok, d4} = clean_pending_bots()
    {:ok, d5} = clean_pending_tros_files()
    {:ok, d6} = clean_invalid_push_tokens()
    {:ok, d7} = clean_expired_invite_codes()
    {:ok, d8} = clean_dead_tros_links(true)

    {:ok, d1 + d2 + d3 + d4 + d5 + d6 + d7 + d8}
  end

  def clean_pending_bots do
    expire_date = Timestamp.shift(days: -1)

    {deleted, nil} =
      Bot
      |> where([b], b.pending == true)
      |> where([b], b.created_at <= ^expire_date)
      |> Repo.delete_all(timeout: :infinity)

    Logger.info("Deleted #{deleted} pending bots created before #{expire_date}")

    {:ok, deleted}
  end

  def clean_traffic_logs do
    expire_date = Timestamp.shift(months: -1)

    {deleted, nil} =
      TrafficLog
      |> where([t], t.created_at <= ^expire_date)
      |> Repo.delete_all(timeout: :infinity)

    Logger.info("Deleted #{deleted} traffic logs created before #{expire_date}")

    {:ok, deleted}
  end

  def clean_notification_logs do
    expire_date = Timestamp.shift(months: -1)

    {deleted, nil} =
      PushLog
      |> where([n], n.created_at <= ^expire_date)
      |> Repo.delete_all(timeout: :infinity)

    Logger.info(
      "Deleted #{deleted} push notification logs created before #{expire_date}"
    )

    {:ok, deleted}
  end

  def clean_invalid_push_tokens do
    expire_date = Timestamp.shift(weeks: -2)

    {deleted, nil} =
      PushToken
      |> where([t], t.valid == false)
      |> where([t], t.enabled_at <= ^expire_date)
      |> Repo.delete_all(timeout: :infinity)

    Logger.info(
      "Deleted #{deleted} invalid push tokens registered before #{expire_date}"
    )

    {:ok, deleted}
  end

  def clean_expired_invite_codes do
    expire_date = Timestamp.shift(weeks: -5)

    {deleted, nil} =
      InviteCode
      |> where([c], c.created_at <= ^expire_date)
      |> Repo.delete_all(timeout: :infinity)

    Logger.info("Deleted #{deleted} expired invitation codes")

    {:ok, deleted}
  end

  def clean_pending_tros_files do
    expire_date = Timestamp.shift(weeks: -1)

    {deleted, nil} =
      Metadata
      |> where([t], t.ready == false)
      |> where([t], t.created_at < ^expire_date)
      |> Repo.delete_all(timeout: :infinity)

    Logger.info(
      "Deleted #{deleted} pending files created before #{expire_date}"
    )

    {:ok, deleted}
  end

  def clean_dead_tros_links(do_clean \\ false) do
    {:ok, d1} = clean_bot_item_image_links(do_clean)
    {:ok, d2} = clean_bot_image_links(do_clean)
    {:ok, d3} = clean_user_avatar_links(do_clean)

    {:ok, d1 + d2 + d3}
  end

  def clean_bot_item_image_links(do_clean) do
    {:ok, cleaned} =
      Repo.transaction(
        fn ->
          Item
          |> where(image: true)
          |> Repo.stream()
          |> Stream.map(&extract_item_image/1)
          |> Stream.filter(fn {_, image} -> image_missing?(image) end)
          |> Stream.each(&purge_missing_item_image(do_clean, &1))
          |> Enum.count()
        end,
        timeout: :infinity
      )

    log_maybe_cleaned(do_clean, cleaned, "bot item image fields with bad links")

    {:ok, cleaned}
  end

  defp extract_item_image(item) do
    image = xpath(item.stanza, ~x"/entry/image/text()"S)
    {item, image}
  end

  defp image_missing?(""), do: false

  defp image_missing?(image_url) do
    case TROS.parse_url(image_url) do
      {:ok, file_id} -> Metadata.get(file_id) == nil
      {:error, _} -> true
    end
  end

  defp purge_missing_item_image(false, {_, _}), do: :ok

  defp purge_missing_item_image(true, {item, image}) do
    content = xpath(item.stanza, ~x"/entry/content/text()"S)

    if String.length(content) > 0 do
      image_tag = "<image>#{image}</image>"
      new_stanza = String.replace(item.stanza, image_tag, "")

      item
      |> Item.changeset(%{stanza: new_stanza, image: false})
      |> Repo.update!()
    else
      Repo.delete!(item)
    end
  end

  defp log_maybe_cleaned(do_clean, count, msg) do
    start =
      if do_clean do
        "Cleaned up"
      else
        "Found"
      end

    Logger.info("#{start} #{count} #{msg}")
  end

  def clean_bot_image_links(do_clean) do
    {:ok, nillified} =
      Repo.transaction(
        fn ->
          Bot
          |> where([b], not is_nil(b.image))
          |> where([b], b.image != "")
          |> Repo.stream()
          |> Stream.filter(&image_missing?(&1.image))
          |> Stream.each(&purge_missing_bot_image(do_clean, &1))
          |> Enum.count()
        end,
        timeout: :infinity
      )

    log_maybe_cleaned(do_clean, nillified, "bot image fields with bad links")

    {:ok, nillified}
  end

  defp purge_missing_bot_image(false, _), do: :ok

  defp purge_missing_bot_image(true, bot) do
    bot
    |> Bot.changeset(%{image: nil})
    |> Repo.update!()
  end

  def clean_user_avatar_links(do_clean) do
    {:ok, nillified} =
      Repo.transaction(
        fn ->
          User
          |> where([u], not is_nil(u.avatar))
          |> where([u], u.avatar != "")
          |> Repo.stream()
          |> Stream.filter(&image_missing?(&1.avatar))
          |> Stream.each(&purge_missing_user_image(do_clean, &1))
          |> Enum.count()
        end,
        timeout: :infinity
      )

    log_maybe_cleaned(do_clean, nillified, "user avatar fields with bad links")

    {:ok, nillified}
  end

  defp purge_missing_user_image(false, _), do: :ok

  defp purge_missing_user_image(true, user) do
    user
    |> User.changeset(%{avatar: nil})
    |> Repo.update!()
  end

  def clean_pending_users do
    expire_date = Timestamp.shift(days: -1)

    {deleted, nil} =
      User
      |> where([u], is_nil(u.handle))
      |> where([u], u.created_at <= ^expire_date)
      |> Repo.delete_all(timeout: :infinity)

    Logger.info(
      "Deleted #{deleted} pending users created before #{expire_date}"
    )

    {:ok, deleted}
  end
end
