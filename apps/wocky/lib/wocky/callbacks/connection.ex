defmodule Wocky.Callbacks.Connection do
  @moduledoc false

  use Dawdle.Handler, only: [Wocky.Presence.ConnectionEvent]

  alias Wocky.Account.User
  alias Wocky.Contacts
  alias Wocky.Events.LocationRequest
  alias Wocky.Location
  alias Wocky.Notifier

  @stale_location_seconds 15

  @impl true
  def handle_event(%{user: user, status: status}) do
    user
    |> Contacts.get_location_sharers()
    |> Enum.each(fn %{user: %User{} = sharer} ->
      case status do
        :connected ->
          Location.inc_watcher_count(sharer)
          maybe_notify_sharer(sharer)

        :disconnected ->
          Location.dec_watcher_count(sharer)
      end
    end)
  end

  defp maybe_notify_sharer(sharer) do
    {:ok, loc} = Location.get_current_user_location(sharer)

    if stale_location?(loc) do
      Notifier.notify(%LocationRequest{to: sharer})
    end
  end

  defp stale_location?(nil), do: true

  defp stale_location?(%{created_at: created_at}) do
    Timex.diff(Timex.now(), created_at, :seconds) >= @stale_location_seconds
  end
end
