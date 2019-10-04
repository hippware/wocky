defmodule Wocky.Callbacks.Presence do
  @moduledoc false

  use Dawdle.Handler, only: [Wocky.Presence.PresenceEvent]

  alias Wocky.Account.User
  alias Wocky.Location

  def handle_event(%{
        contact: %User{id: user_id, presence: presence} = user,
        recipient_id: user_id
      }) do
    user
    |> Location.get_location_sharers()
    |> Enum.each(fn %{user: %User{id: id}} ->
      case presence.status do
        :online -> Location.inc_watcher_count(id)
        :offline -> Location.dec_watcher_count(id)
      end
    end)
  end

  def handle_event(_), do: :ok
end
