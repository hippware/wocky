defmodule Wocky.Callbacks.Relationship do
  @moduledoc "DB callback handler for location shares"

  use DawdleDB.Handler, type: Wocky.Contacts.Relationship

  alias Wocky.Contacts
  alias Wocky.Contacts.Share
  alias Wocky.Events.LocationShare
  alias Wocky.Events.LocationShareEnd
  alias Wocky.Events.LocationShareEndSelf
  alias Wocky.Events.UserBefriend
  alias Wocky.Notifier
  alias Wocky.Repo.Hydrator

  @impl true
  def handle_insert(new) do
    _ = Contacts.refresh_share_cache(new.user_id)

    if new.state == :friend do
      notify_befriend(new)
    end

    if new.share_type == :always do
      notify_share_start(new)
    end
  end

  @impl true
  def handle_update(new, old) do
    _ = Contacts.refresh_share_cache(new.user_id)

    if new.state == :friend and old.state != :friend do
      notify_befriend(new)

      if new.share_type != :disabled do
        notify_share_start(new)
      end
    end

    case {new.share_type, old.share_type} do
      {stype, stype} ->
        :ok

      {_, :disabled} ->
        if new.state == :friend and old.state == :friend do
          notify_share_start(new)
        end

      {:disabled, _} ->
        notify_share_end(new)

      {_, _} ->
        :ok
    end
  end

  @impl true
  def handle_delete(old) do
    _ = Contacts.refresh_share_cache(old.user_id)

    if old.share_type != :disabled do
      notify_share_end(old)
    end
  end

  defp notify_share_start(share) do
    Hydrator.with_assocs(share, [:user, :contact], fn rec ->
      %LocationShare{
        to: rec.contact,
        from: rec.user,
        expires_at: Share.make_expiry(),
        share_id: rec.share_id,
        share_type: rec.share_type,
        other_user_share_type: Contacts.cached_share_type(rec.contact, rec.user)
      }
      |> Notifier.notify()
    end)
  end

  defp notify_share_end(share) do
    Hydrator.with_assocs(share, [:user, :contact], fn rec ->
      %LocationShareEnd{
        to: rec.contact,
        from: rec.user,
        share_id: share.share_id
      }
      |> Notifier.notify()

      if Confex.get_env(:wocky, :location_share_end_self) do
        %LocationShareEndSelf{
          to: rec.user,
          from: rec.contact,
          share_id: share.share_id
        }
        |> Notifier.notify()
      end
    end)
  end

  defp notify_befriend(relationship) do
    Hydrator.with_assocs(relationship, [:user, :contact], fn rec ->
      %UserBefriend{
        to: rec.user,
        from: rec.contact
      }
      |> Notifier.notify()
    end)
  end
end
