defmodule Wocky.Repo.Cleaner do
  @moduledoc "Functions that keep the database nice and clean."

  import Ecto.Query

  alias Wocky.Account.User
  alias Wocky.Location.BotEvent
  alias Wocky.Notifier.Push.Token, as: PushToken
  alias Wocky.POI.Bot
  alias Wocky.POI.Item
  alias Wocky.Repo
  alias Wocky.Repo.Timestamp
  alias Wocky.TROS
  alias Wocky.TROS.Metadata
  alias Wocky.UserInvite.InviteCode

  require Logger

  def clean_all do
    {:ok, d1} = clean_pending_users()
    {:ok, d2} = clean_pending_bots()
    {:ok, d3} = clean_pending_tros_files()
    {:ok, d4} = clean_invalid_push_tokens()
    {:ok, d5} = clean_expired_invite_codes()
    {:ok, d6} = clean_dead_tros_links(true)
    {:ok, d7} = clean_transient_users()
    {:ok, d8} = clean_stale_bot_events()

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
          |> where([i], not is_nil(i.image_url))
          |> Repo.stream()
          |> Stream.filter(&image_missing?(&1.image_url))
          |> Stream.each(&purge_missing_item_image(do_clean, &1))
          |> Enum.count()
        end,
        timeout: :infinity
      )

    log_maybe_cleaned(do_clean, cleaned, "bot item image fields with bad links")

    {:ok, cleaned}
  end

  defp image_missing?(""), do: false

  defp image_missing?(image_url) do
    case TROS.parse_url(image_url) do
      {:ok, file_id} -> Repo.get(Metadata, file_id) == nil
      {:error, _} -> true
    end
  end

  defp purge_missing_item_image(false, _), do: :ok

  defp purge_missing_item_image(true, item) do
    if item.content do
      item
      |> Item.changeset(%{image_url: nil})
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

    :ok
  end

  def clean_bot_image_links(do_clean) do
    {:ok, nillified} =
      Repo.transaction(
        fn ->
          Bot
          |> where([b], not is_nil(b.image_url))
          |> where([b], b.image_url != "")
          |> Repo.stream()
          |> Stream.filter(&image_missing?(&1.image_url))
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
    |> Bot.changeset(%{image_url: nil})
    |> Repo.update!()
  end

  def clean_user_avatar_links(do_clean) do
    {:ok, nillified} =
      Repo.transaction(
        fn ->
          User
          |> where([u], not is_nil(u.image_url))
          |> where([u], u.image_url != "")
          |> Repo.stream()
          |> Stream.filter(&image_missing?(&1.image_url))
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
    |> User.changeset(%{image_url: nil})
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

  def clean_transient_users do
    case Confex.get_env(:wocky, :expire_transient_users_after_days) do
      nil ->
        Logger.info("Transient user deletion disabled - skipping")

        {:ok, 0}

      days ->
        expire_date = Timestamp.shift(days: -days)

        {deleted, nil} =
          User
          |> where([u], u.transient)
          |> where([u], u.created_at <= ^expire_date)
          |> Repo.delete_all(timeout: :infinity)

        Logger.info(
          "Deleted #{deleted} transient users created before #{expire_date}"
        )

        {:ok, deleted}
    end
  end

  def clean_stale_bot_events do
    expire_date = Timestamp.shift(months: -6)

    {deleted, nil} =
      BotEvent
      |> where([u], u.created_at <= ^expire_date)
      |> Repo.delete_all(timeout: :infinity)

    Logger.info("Deleted #{deleted} bot_events created before #{expire_date}")

    {:ok, deleted}
  end
end
