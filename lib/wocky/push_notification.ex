defmodule Wocky.PushNotification do

  require Logger

  alias Wocky.JID
  alias Wocky.PushNotificationBroker
  alias __MODULE__, as: PushNotification

  @type jid :: JID.jid
  @type platform :: binary
  @type device_id :: binary

  defstruct [
    :from,
    :to,
    :body
  ]

  @type t :: %PushNotification{
    from:   jid | nil,
    to:     jid,
    body:   binary
  }

  @spec send(jid | nil, jid, binary) :: :ok
  def send(from, to, body) do
    PushNotificationBroker.send(
      %PushNotification{from: from, to: to, body: body})
  end

  @spec enable(jid, platform, device_id) :: :ok | {:error, term}
  def enable(jid, platform, device_id) do
    bjid = JID.to_binary(jid)
    Logger.debug("Registering device '#{device_id}' for user '#{bjid}'")
    case handler().register(bjid, platform, device_id) do
      {:ok, endpoint} ->
        {user, server, resource} = JID.to_lower(jid)
        created_at = :wocky_db.now_to_timestamp(:os.timestamp())
        :ok = :wocky_db.insert(server, :device, %{user => user,
                                                  server => server,
                                                  resource => resource,
                                                  platform => platform,
                                                  device_id => device_id,
                                                  endpoint => endpoint,
                                                  created_at => created_at})
        :ok;
      {:error, _} = error ->
        error
    end
  end

  @spec disable(jid) :: :ok
  def disable(jid) do
    {user, server, resource} = JID.to_lower(jid)
    :ok = :wocky_db.delete(server, :device, :all, %{user => user,
                                                    server => server,
                                                    resource => resource})
  end

  @spec delete(jid) :: :ok
  def delete(jid) do
    {user, server} = JID.to_lus(jid)
    :wocky_db.delete(server, :device, :all, %{user => user, server => server})
  end

  defp handler() do
    {:ok, handler} = :application.get_env(:wocky, :notification_handler)
    handler
  end

end
