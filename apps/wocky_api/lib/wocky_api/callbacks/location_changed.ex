defmodule WockyAPI.Callbacks.LocationChanged do
  @moduledoc false

  use Dawdle.Handler, only: [Wocky.Location.UserLocation.LocationChangedEvent]

  alias WockyAPI.Resolvers.User

  def handle_event(%{user: user, location: location}) do
    User.notify_location(user, location)
  end
end
