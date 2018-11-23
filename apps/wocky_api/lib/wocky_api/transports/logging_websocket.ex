defmodule WockyAPI.Transports.LoggingWebSocket do
  @moduledoc """
  This is a wrapper module that sits on top of Phoenix.Transports.WebSocket
  to add wocky traffic logging
  """

  @base Phoenix.Transports.WebSocket

  @behaviour Phoenix.Socket.Transport

  import Plug.Conn, only: [fetch_query_params: 1, send_resp: 3]

  alias Phoenix.Socket.Transport
  alias Wocky.{TrafficLog, User}

  # Phoenix magically stuffs the `cowboy` config element in based on the
  # module being used. So we have to set it explicitly.
  def default_config do
    [{:cowboy, Phoenix.Endpoint.CowboyWebSocket} | @base.default_config()]
  end

  def set_user(transport_pid, user), do: send(transport_pid, {:set_user, user})

  # This impelmentation is just a copy of Phoenix.Transports.Websocket.init/2,
  # but needs to be repeated here so that __MODULE__ points to this module.
  def init(%Plug.Conn{method: "GET"} = conn, {endpoint, handler, transport}) do
    {_, opts} = handler.__transport__(transport)

    conn =
      conn
      |> code_reload(opts, endpoint)
      |> fetch_query_params()
      |> Transport.transport_log(opts[:transport_log])
      |> Transport.force_ssl(handler, endpoint, opts)
      |> Transport.check_origin(handler, endpoint, opts)

    case conn do
      %{halted: false} = conn ->
        params = conn.params
        serializer = Keyword.fetch!(opts, :serializer)

        case Transport.connect(
               endpoint,
               handler,
               transport,
               __MODULE__,
               serializer,
               params
             ) do
          {:ok, socket} ->
            {:ok, conn, {__MODULE__, {socket, opts}}}

          :error ->
            conn = send_resp(conn, 403, "")
            {:error, conn}
        end

      %{halted: true} = conn ->
        {:error, conn}
    end
  end

  def init(conn, params), do: @base.init(conn, params)

  def ws_init(params), do: @base.ws_init(params)

  def ws_handle(opcode, payload, state) do
    log(payload, state, false)
    @base.ws_handle(opcode, payload, state)
  end

  def ws_info({:socket_push, :text, payload} = params, state) do
    log(payload |> to_string(), state, true)
    @base.ws_info(params, state)
  end

  def ws_info({:set_user, user}, state),
    do: {:ok, Map.put(state, :user, user)}

  def ws_info(params, state), do: @base.ws_info(params, state)

  def ws_terminate(reason, state), do: @base.ws_terminate(reason, state)

  def ws_close(state), do: @base.ws_close(state)

  defp code_reload(conn, opts, endpoint) do
    reload? = Keyword.get(opts, :code_reloader, endpoint.config(:code_reloader))
    if reload?, do: Phoenix.CodeReloader.reload!(endpoint)

    conn
  end

  defp log(packet, state, incoming?) do
    context = state.socket.assigns[:absinthe][:opts][:context]
    user = state[:user]

    %{
      user_id: user && user.id,
      device: "GraphQL",
      host: context.host,
      ip: inspect(context.transport_pid),
      incoming: incoming?,
      packet: packet
    }
    |> TrafficLog.put(User.hippware?(user))
  end
end
