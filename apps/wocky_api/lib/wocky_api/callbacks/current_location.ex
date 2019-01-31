defmodule WockyAPI.Callbacks.CurrentLocation do
  @moduledoc """
  Callbacks for DB user current location changes
  """

  use Wocky.Watcher,
    type: Wocky.User.CurrentLocation,
    events: [:insert, :update]

  alias Wocky.User.CurrentLocation
  alias WockyAPI.Resolvers.User, as: UserResolver

  def handle_insert(%Event{new: %CurrentLocation{} = location}) do
    UserResolver.notify_location(location)
  end

  def handle_insert(_), do: :ok

  def handle_update(%Event{new: %CurrentLocation{} = location}) do
    UserResolver.notify_location(location)
  end

  def handle_update(_), do: :ok
end
