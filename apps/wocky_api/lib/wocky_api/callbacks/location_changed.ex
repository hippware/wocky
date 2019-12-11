defmodule WockyAPI.Callbacks.LocationChanged do
  @moduledoc false

  use Dawdle.Handler, only: [Wocky.Location.UserLocation.LocationChangedEvent]

  alias WockyAPI.Resolvers.Contact

  def handle_event(%{user: user, location: location}) do
    Contact.notify_location(user, location)
  end
end
