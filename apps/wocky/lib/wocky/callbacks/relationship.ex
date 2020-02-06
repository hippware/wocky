defmodule Wocky.Callbacks.Relationship do
  @moduledoc "DB callback handler for location shares"

  use DawdleDB.Handler, type: Wocky.Contacts.Relationship

  alias Wocky.Contacts
  alias Wocky.Contacts.Relationship
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
      handle_new_friendship(new)
    end

    handle_share_notifications(new, old)
  end

  @impl true
  def handle_delete(old) do
    _ = Contacts.refresh_share_cache(old.user_id)

    if old.share_type != :disabled do
      notify_share_end(old)
    end
  end

  defp handle_new_friendship(new) do
    notify_befriend(new)

    if new.share_type != :disabled do
      notify_share_start(new)
    end
  end

  # Share type is unchanged
  defp handle_share_notifications(%Relationship{share_type: s}, %Relationship{
         share_type: s
       }),
       do: :ok

  # Share has been enabled for a friend
  defp handle_share_notifications(
         %Relationship{state: :friend} = new,
         %Relationship{state: :friend, share_type: :disabled}
       ),
       do: notify_share_start(new)

  # Share has been disabled
  defp handle_share_notifications(
         %Relationship{share_type: :disabled} = new,
         _
       ),
       do: notify_share_end(new)

  # Any other case
  defp handle_share_notifications(_, _), do: :ok

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
