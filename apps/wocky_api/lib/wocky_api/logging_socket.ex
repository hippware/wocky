defmodule WockyAPI.LoggingSocket do
  alias Wocky.Audit
  alias Wocky.Audit.TrafficLog

  defmacro __using__(opts) do
    # Macro modelled on Phoenix.Socket.__using__
    quote do
      ## User API

      import Phoenix.Socket

      alias Phoenix.Socket
      alias WockyAPI.LoggingSocket
      alias WockyAPI.Metrics

      @behaviour Phoenix.Socket
      @before_compile Phoenix.Socket
      Module.register_attribute(__MODULE__, :phoenix_channels, accumulate: true)
      @phoenix_log Keyword.get(unquote(opts), :log, :info)

      ## Callbacks

      @behaviour Phoenix.Socket.Transport

      @doc false
      @impl true
      def child_spec(opts), do: Socket.__child_spec__(__MODULE__, opts)

      @doc false
      @impl true
      def connect(map), do: Socket.__connect__(__MODULE__, map, @phoenix_log)

      @doc false
      @impl true
      def init(state) do
        with {:ok, {channels, socket}} <- Socket.__init__(state) do
          if socket.transport == :websocket do
            Metrics.add_ws_connection(socket.transport_pid)

            absinthe_assigns =
              socket.assigns
              |> Map.get(:absinthe, %{})
              |> put_in([:opts, :context, :transport_pid], socket.transport_pid)

            {:ok, {channels, assign(socket, :absinthe, absinthe_assigns)}}
          else
            {:ok, {channels, socket}}
          end
        end
      end

      @doc false
      @impl true
      def handle_in({payload, opcode: :text} = message, state) do
        LoggingSocket.log(payload, state, false)

        message
        |> Socket.__in__(state)
        |> LoggingSocket.maybe_log_reply()
      end

      def handle_in(message, state), do: Socket.__in__(message, state)

      @doc false
      @impl true
      def handle_info({:socket_push, :text, payload} = message, state) do
        LoggingSocket.log(payload |> to_string(), state, true)
        Socket.__info__(message, state)
      end

      def handle_info({:set_user_info, user, device}, {channels, socket}) do
        new_socket =
          socket
          |> assign(:user, user)
          |> assign(:device, device)

        {:ok, {channels, new_socket}}
      end

      def handle_info(message, state), do: Socket.__info__(message, state)

      @doc false
      @impl true
      def terminate(reason, state), do: Socket.__terminate__(reason, state)
    end
  end

  def set_user_info(transport_pid, user, device),
    do: send(transport_pid, {:set_user_info, user, device})

  def log(payload, {_channels, socket}, incoming?) do
    context = socket.assigns[:absinthe][:opts][:context]
    user = socket.assigns[:user]
    device = socket.assigns[:device]

    %TrafficLog{
      user_id: user && user.id,
      device: device,
      host: context.host,
      ip: context.peer,
      incoming: incoming?,
      packet: to_string(payload),
      created_at: DateTime.utc_now()
    }
    |> Audit.log_traffic(user)
  end

  def maybe_log_reply({:reply, :ok, {:text, payload}, state} = reply) do
    log(payload, state, true)
    reply
  end

  def maybe_log_reply(other), do: other
end
