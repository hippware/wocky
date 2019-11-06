defmodule Wocky.Callbacks.Friend do
  @moduledoc "DB callback handler for location shares"

  use DawdleDB.Handler, type: Wocky.Friends.Friend

  alias Wocky.Events.LocationShare
  alias Wocky.Events.LocationShareEnd
  alias Wocky.Events.LocationShareEndSelf
  alias Wocky.Friends
  alias Wocky.Friends.Share
  alias Wocky.Notifier
  alias Wocky.Repo.Hydrator

  def handle_insert(%{share_type: stype} = new) when stype == :always,
    do: notify_share_start(new)

  def handle_insert(new), do: Friends.refresh_share_cache(new.user_id)

  def handle_update(%{share_type: stype} = new, %{share_type: stype}),
    do: Friends.refresh_share_cache(new.user_id)

  def handle_update(%{share_type: :always} = new, _old),
    do: notify_share_start(new)

  def handle_update(%{share_type: :disabled} = new, _old),
    do: notify_share_end(new)

  def handle_update(new, _old), do: Friends.refresh_share_cache(new.user_id)

  def handle_delete(%{share_type: stype} = old) when stype != :disabled,
    do: notify_share_end(old)

  def handle_delete(old), do: Friends.refresh_share_cache(old.user_id)

  defp notify_share_start(share) do
    Hydrator.with_assocs(share, [:user, :contact], fn rec ->
      %LocationShare{
        to: rec.contact,
        from: rec.user,
        expires_at: Share.make_expiry(),
        share_id: share.share_id
      }
      |> Notifier.notify()
    end)

    Friends.refresh_share_cache(share.user_id)
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

    Friends.refresh_share_cache(share.user_id)
  end
end
