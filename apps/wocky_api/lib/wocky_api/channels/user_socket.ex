defmodule WockyAPI.UserSocket do
  use Phoenix.Socket

  use Absinthe.Phoenix.Socket,
    schema: WockyAPI.Schema,
    pipeline: {WockyAPI.Pipeline, :channel_pipeline}

  @max_complexity Application.fetch_env!(:wocky_api, :max_graphql_complexity)

  ## Channels
  # channel "room:*", WockyAPI.RoomChannel

  ## Transports
  transport :websocket, WockyAPI.Transports.LoggingWebSocket
  # transport :longpoll, Phoenix.Transports.LongPoll

  # Socket params are passed from the client and can
  # be used to verify and authenticate a user. After
  # verification, you can put default assigns into
  # the socket that will be set for all channels, ie
  #
  #     {:ok, assign(socket, :user_id, verified_user_id)}
  #
  # To deny connection, return `:error`.
  #
  # See `Phoenix.Token` documentation for examples in
  # performing token verification on connect.
  def connect(_params, socket) do
    socket =
      Absinthe.Phoenix.Socket.put_options(
        socket,
        context: %{
          host: host(),
          transport_pid: socket.transport_pid
        },
        analyze_complexity: true,
        max_complexity: @max_complexity
      )

    {:ok, socket}
  end

  defp host do
    {:ok, host} = :inet.gethostname()
    to_string(host)
  end

  # Socket id's are topics that allow you to identify all sockets for a given
  # user:
  #
  #     def id(socket), do: "user_socket:#{socket.assigns.user_id}"
  #
  # Would allow you to broadcast a "disconnect" event and terminate
  # all active sockets and channels for a given user:
  #
  #     WockyAPI.Endpoint.broadcast("user_socket:#{user.id}", "disconnect", %{})
  #
  # Returning `nil` makes this socket anonymous.
  def id(_socket), do: nil
end
