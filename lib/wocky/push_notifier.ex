defmodule Wocky.PushNotifier do
  @moduledoc "Behavior and interface to client push notification services"

  use Wocky.JID

  alias Wocky.Device
  alias Wocky.User

  require Logger

  @type endpoint :: binary

  # ===================================================================
  # Behaviour definition

  @callback init :: :ok

  @callback enable(user :: User.id,
                   server :: User.server,
                   resource :: User.resource,
                   platform :: Device.platform,
                   device :: Device.device) ::
    {:ok, endpoint :: endpoint} | {:error, reason :: any}

  @callback disable(endpoint) :: :ok

  @callback push(body :: binary, to :: endpoint) ::
    :ok | {:error, reason :: any}

  # ===================================================================
  # API

  @spec init(list) :: :ok
  def init(args) do
    backend_type = Keyword.get(args, :notification_system, :none)
    backend_module = case backend_type do
      :aws ->
        :ok = Logger.info("SNS notifications enabled")
        Wocky.PushNotifier.SNSBackend
      :test ->
        :ok = Logger.info("Notification testing system enabled")
        Wocky.PushNotifier.TestBackend
      :none ->
        :ok = Logger.info("Notifications disabled")
        Wocky.PushNotifier.NullBackend
    end
    Application.put_env(:wocky, :push_notification_backend, backend_module)
    backend_module.init()
  end

  @spec enable(JID.t, Device.platform, Device.device) :: {:ok, endpoint}
                                                       | {:error, any}
  def enable(jid, platform, device_id) do
    jid(luser: user, lserver: server, lresource: resource) = jid

    case backend().enable(user, server, resource, platform, device_id) do
      {:ok, endpoint} = result ->
        Device.update(
          %Device{
            user: user,
            server: server,
            resource: resource,
            platform: platform,
            device_id: device_id,
            endpoint: endpoint
          }
        )
        result

      {:error, _} = error ->
        error
    end
  end

  @spec disable(JID.t) :: :ok
  def disable(jid(luser: user, lserver: server, lresource: resource)) do
    user
    |> Device.get_endpoint(server, resource)
    |> backend().disable

    Device.delete(user, server, resource)
  end

  @spec delete(JID.t) :: :ok
  def delete(jid(luser: user, lserver: server)) do
    for endpoint <- Device.get_all_endpoints(user, server) do
      backend().disable(endpoint)
    end

    Device.delete_all(user, server)
  end

  @spec push(JID.t, binary) :: :ok
  def push(jid(luser: user, lserver: server, lresource: res), message) do
    user
    |> Device.get_endpoint(server, res)
    |> do_push(message)
  end

  @spec push_all(JID.t, binary) :: :ok
  def push_all(jid(luser: user, lserver: server), message) do
    for endpoint <- Device.get_all_endpoints(user, server) do
      do_push(endpoint, message)
    end
    :ok
  end

  # ===================================================================
  # Helpers

  defp do_push(nil, _message), do: :ok
  defp do_push(endpoint, message) do
    message
    |> backend().push(endpoint)
    |> log_result(endpoint, message)
  end

  defp log_result({:error, e}, endpoint, message) do
    Logger.warn("""
    Notification error '#{inspect(e)}' while sending message to endpoint \
    '#{endpoint}' with body '#{message}'\
    """)
  end

  defp log_result(:ok, endpoint, message) do
    Logger.debug("""
    Notification sent to endpoint '#{endpoint}' with body '#{message}'\
    """)
  end

  defp backend, do: Application.fetch_env!(:wocky, :push_notification_backend)
end
