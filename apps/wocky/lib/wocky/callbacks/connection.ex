defmodule Wocky.Callbacks.Connection do
  @moduledoc false

  use Dawdle.Handler, only: [Wocky.Presence.ConnectionEvent]

  alias Wocky.Account.User
  alias Wocky.Location

  def handle_event(%{user: user, status: status}) do
    user
    |> Location.get_location_sharers()
    |> Enum.each(fn %{user: %User{id: id}} ->
      case status do
        :connected -> Location.inc_watcher_count(id)
        :disconnected -> Location.dec_watcher_count(id)
      end
    end)
  end
end
