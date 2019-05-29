defmodule Wocky.Callbacks.LocationShare do
  @moduledoc "DB callback handler for location shares"

  use DawdleDB.Handler, type: Wocky.Location.Share

  alias Wocky.Events.{LocationShare, LocationShareEnd}
  alias Wocky.Location
  alias Wocky.Notifier
  alias Wocky.Repo

  def handle_insert(new) do
    new = Repo.preload(new, [:user, :shared_with])

    if new.user != nil && new.shared_with != nil do
      %LocationShare{
        to: new.shared_with,
        from: new.user,
        expires_at: new.expires_at
      }
      |> Notifier.notify()
    end

    Location.refresh_share_cache(new.user_id)
  end

  def handle_update(_old, new) do
    Location.refresh_share_cache(new.user_id)
  end

  def handle_delete(old) do
    old = Repo.preload(old, [:user, :shared_with])

    if old.user != nil && old.shared_with != nil do
      %LocationShareEnd{
        to: old.shared_with,
        from: old.user
      }
      |> Notifier.notify()
    end

    Location.refresh_share_cache(old.user_id)
  end
end
