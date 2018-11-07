defmodule WockyAPI.Callbacks.User do
  @moduledoc """
  Callbacks for DB user changes
  """

  use Wocky.Watcher, type: Wocky.User, events: [:update]

  alias Wocky.User
  alias WockyAPI.Resolvers.User, as: UserResolver

  def handle_update(%Event{new: %User{} = user}) do
    UserResolver.notify_followers(user)
  end

  def handle_update(_), do: :ok
end
