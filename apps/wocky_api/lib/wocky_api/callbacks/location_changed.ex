defmodule WockyAPI.Callbacks.LocationChanged do
  @moduledoc false

  use Dawdle.Handler, only: [Wocky.Location.UserLocation.LocationChangedEvent]

  alias WockyAPI.Resolvers.Friend

  def handle_event(%{user: user, location: location}) do
    Friend.notify_location(user, location)
  end
end
