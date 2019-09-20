defmodule Wocky.Callbacks.LocationShare do
  @moduledoc "DB callback handler for location shares"

  use DawdleDB.Handler, type: Wocky.Location.Share

  alias Wocky.Events.LocationShare
  alias Wocky.Events.LocationShareEnd
  alias Wocky.Location
  alias Wocky.Notifier
  alias Wocky.Repo.Hydrator

  def handle_insert(new) do
    Hydrator.with_assocs(new, [:user, :shared_with], fn rec ->
      %LocationShare{
        to: rec.shared_with,
        from: rec.user,
        expires_at: rec.expires_at
      }
      |> Notifier.notify()
    end)

    Location.refresh_share_cache(new.user_id)
  end

  def handle_update(new, _old) do
    Location.refresh_share_cache(new.user_id)
  end

  def handle_delete(old) do
    Hydrator.with_assocs(old, [:user, :shared_with], fn rec ->
      %LocationShareEnd{
        to: rec.shared_with,
        from: rec.user
      }
      |> Notifier.notify()
    end)

    Location.refresh_share_cache(old.user_id)
  end
end
