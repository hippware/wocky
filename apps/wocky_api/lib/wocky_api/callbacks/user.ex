defmodule WockyAPI.Callbacks.User do
  @moduledoc """
  Callbacks for DB user changes
  """

  use DawdleDB.Handler, type: Wocky.Account.User

  alias Phoenix.Socket.Broadcast
  alias Wocky.Account.User
  alias Wocky.Presence
  alias WockyAPI.Resolvers.User, as: UserResolver

  def handle_update(%User{} = user, _old) do
    UserResolver.notify_friends(user)
  end

  def handle_update(_new, _old), do: :ok

  def handle_delete(user) do
    user
    |> Presence.get_sockets()
    |> Enum.each(&send(&1, %Broadcast{event: "disconnect"}))
  end
end
