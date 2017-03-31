defmodule Wocky.Device do
  @moduledoc "Represents a client device that the user uses to connect"

  use Wocky.Repo.Model

  alias Wocky.Repo.Timestamp
  alias Wocky.User
  alias __MODULE__, as: Device

  @foreign_key_type :binary_id
  @primary_key false
  schema "devices" do
    field :user_id,   :binary_id, null: false, primary_key: true
    field :resource,  :string, null: false, primary_key: true
    field :platform,  :string, null: false
    field :device,    :string, null: false
    field :endpoint,  :string, null: false

    timestamps()

    belongs_to :user, User, define_field: false
  end

  @type device :: binary
  @type platform :: :apple | :google
  @type endpoint :: binary

  @spec update(User.id, User.resource, platform, device, endpoint) :: :ok
  def update(user_id, resource, platform, device, endpoint) do
    %Device{
      user_id: user_id,
      resource: resource,
      platform: to_string(platform),
      device: device,
      endpoint: endpoint
    }
    |> Repo.insert!(on_conflict: :replace_all)

    :ok
  end

  def with_user(query, user_id) do
    from d in query, where: d.user_id == ^user_id
  end

  def and_resource(query, resource) do
    from d in query, where: d.resource == ^resource
  end

  def select_endpoint(query) do
    from d in query, select: d.endpoint
  end

  @doc "Return the endpoint assigned to the specified user and resource."
  @spec get_endpoint(User.id, User.resource) :: nil | endpoint
  def get_endpoint(user_id, resource) do
    Device
    |> with_user(user_id)
    |> and_resource(resource)
    |> select_endpoint
    |> Repo.one
  end

  @doc """
  Returns all endpoints currently assigned to resources belonging to the
  specified user.
  """
  @spec get_all_endpoints(User.id) :: [endpoint]
  def get_all_endpoints(user_id) do
    Device
    |> with_user(user_id)
    |> select_endpoint
    |> Repo.all
  end

  @doc """
  Deletes any device currently assigned to the specified user and resource.
  """
  @spec delete(User.id, User.resource) :: :ok
  def delete(user_id, resource) do
    Device
    |> with_user(user_id)
    |> and_resource(resource)
    |> Repo.delete_all

    :ok
  end

  @doc "Deletes all devices currently assigned to the specified user"
  @spec delete_all(User.id) :: :ok
  def delete_all(user_id) do
    Device
    |> with_user(user_id)
    |> Repo.delete_all

    :ok
  end
end
