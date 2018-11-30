defmodule WockyAPI.LoggingSocket do
  alias Wocky.{TrafficLog, User}

  defmacro __using__(opts) do
    # Macro modelled on Phoenix.Socket.__using__
    quote do
      ## User API

      import Phoenix.Socket

      alias Phoenix.Socket
      alias WockyAPI.LoggingSocket

      @behaviour Phoenix.Socket
      @before_compile Phoenix.Socket
      Module.register_attribute(__MODULE__, :phoenix_channels, accumulate: true)
      @phoenix_log Keyword.get(unquote(opts), :log, :info)

      ## Callbacks

      @behaviour Phoenix.Socket.Transport

      @doc false
      def child_spec(opts), do: Socket.__child_spec__(__MODULE__, opts)

      @doc false
      def connect(map), do: Socket.__connect__(__MODULE__, map, @phoenix_log)

      @doc false
      def init(state) do
        with {:ok, {channels, socket}} <- Socket.__init__(state) do
          absinthe_assigns =
            socket.assigns
            |> Map.get(:absinthe, %{})
            |> put_in([:opts, :context, :transport_pid], socket.transport_pid)

          {:ok, {channels, assign(socket, :absinthe, absinthe_assigns)}}
        end
      end

      @doc false
      def handle_in({payload, opcode: :text} = message, state) do
        LoggingSocket.log(payload, state, false)

        message
        |> Socket.__in__(state)
        |> LoggingSocket.maybe_log_reply()
      end

      def handle_in(message, state), do: Socket.__in__(message, state)

      @doc false
      def handle_info({:socket_push, :text, payload} = message, state) do
        LoggingSocket.log(payload |> to_string(), state, true)
        Socket.__info__(message, state)
      end

      def handle_info({:set_user, user}, {channels, socket}),
        do: {:ok, {channels, assign(socket, :user, user)}}

      def handle_info(message, state), do: Socket.__info__(message, state)

      @doc false
      def terminate(reason, state), do: Socket.__terminate__(reason, state)
    end
  end

  def set_user(transport_pid, user), do: send(transport_pid, {:set_user, user})

  def log(payload, {_channels, socket}, incoming?) do
    context = socket.assigns[:absinthe][:opts][:context]
    user = socket.assigns[:user]

    %{
      user_id: user && user.id,
      device: "GraphQL",
      host: context.host,
      ip: context.peer,
      incoming: incoming?,
      packet: to_string(payload)
    }
    |> TrafficLog.put(User.hippware?(user))
  end

  def maybe_log_reply({:reply, :ok, {:text, payload}, state} = reply) do
    log(payload, state, true)
    reply
  end

  def maybe_log_reply(other), do: other
end
