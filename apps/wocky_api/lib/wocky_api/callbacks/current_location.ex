defmodule WockyAPI.Callbacks.CurrentLocation do
  @moduledoc """
  Callbacks for DB user current location changes
  """

  use DawdleDB.Handler, type: Wocky.User.CurrentLocation

  alias Wocky.User.CurrentLocation
  alias WockyAPI.Resolvers.User, as: UserResolver

  def handle_insert(%CurrentLocation{} = location) do
    UserResolver.notify_location(location)
  end

  def handle_insert(_), do: :ok

  def handle_update(%CurrentLocation{} = location, _) do
    UserResolver.notify_location(location)
  end

  def handle_update(_, _), do: :ok
end
