defmodule WockyAPI.Middleware.Socket do
  @moduledoc "Middleware for handling socket-related operations"

  alias Wocky.Presence
  alias WockyAPI.LoggingSocket
  alias WockyAPI.Metrics

  @behaviour Absinthe.Middleware

  @impl true
  def call(resolution, :authenticated) do
    with %{value: %{user: user, device: device}} <- resolution do
      transport_pid = resolution.context[:transport_pid]

      if transport_pid do
        LoggingSocket.set_user_info(transport_pid, user, device)
        Metrics.add_auth_connection(transport_pid)
        Presence.register_socket(user, transport_pid)
      end

      %{resolution | context: Map.put(resolution.context, :current_user, user)}
    end
  end
end
