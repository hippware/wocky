defmodule Wocky.Device do
  @moduledoc "Represents a client device that the user uses to connect"

  alias Wocky.User
  alias __MODULE__, as: Device

  defstruct [:user, :server, :resource, :platform, :device_id, :endpoint]

  @type t :: %Device{}
  @type device :: binary
  @type platform :: binary
  @type endpoint :: binary

  @spec update(Device.t) :: :ok
  def update(device) do
    device
    |> Map.from_struct
    |> add_created_timestamp()
    |> do_insert(device.server)
  end

  defp add_created_timestamp(map) do
    created_at = :wocky_db.now_to_timestamp(:os.timestamp())
    Map.put(map, :created_at, created_at)
  end

  defp do_insert(map, server) do
    :ok = :wocky_db.insert(server, :device, map)
  end

  @doc "Return the endpoint assigned to the specified user and resource."
  @spec get_endpoint(User.id, User.server, User.resource) ::
    :not_found | endpoint
  def get_endpoint(user, server, resource) do
    :wocky_db.select_one(server, :device, :endpoint, %{user: user,
                                                       server: server,
                                                       resource: resource})
  end

  @doc """
  Returns all endpoints currently assigned to resources belonging to the
  specified user.
  """
  @spec get_all_endpoints(User.id, User.server) :: [endpoint]
  def get_all_endpoints(user, server) do
    :wocky_db.select_column(server, :device, :endpoint, %{user: user,
                                                          server: server})
  end

  @doc """
  Deletes any device currently assigned to the specified user and resource.
  """
  @spec delete(User.id, User.server, User.resource) :: :ok
  def delete(user, server, resource) do
    :wocky_db.delete(server, :device, :all, %{user: user,
                                              server: server,
                                              resource: resource})
  end

  @doc "Deletes all devices currently assigned to the specified user"
  @spec delete_all(User.id, User.server) :: :ok
  def delete_all(user, server) do
    :wocky_db.delete(server, :device, :all, %{user: user, server: server})
  end
end
