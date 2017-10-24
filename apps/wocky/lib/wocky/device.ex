defmodule Wocky.Device do
  @moduledoc "Represents a client device that the user uses to connect"

  use Wocky.Repo.Model

  alias Wocky.Repo
  alias Wocky.User
  alias __MODULE__, as: Device

  @foreign_key_type :binary_id
  @primary_key false
  schema "devices" do
    field :user_id,   :binary_id, null: false, primary_key: true
    field :resource,  :string, null: false, primary_key: true
    field :platform,  :string, null: false
    field :token,     :string, null: false
    field :invalid,   :boolean, null: false, default: false
    field :feedback,  :boolean, null: false, default: false

    timestamps()

    belongs_to :user, User, define_field: false
  end

  @type t :: %Device{
    user_id:  User.id,
    resource: binary,
    platform: binary,
    token:    binary
  }

  @type token :: binary
  @type platform :: binary # :apple | :google

  @spec update(User.id, User.resource, platform, token) :: :ok
  def update(user_id, resource, platform, token) do
    device = %Device{
      user_id: user_id,
      resource: resource,
      platform: to_string(platform),
      token: token
    }

    Repo.insert!(device, on_conflict: :replace_all,
                         conflict_target: [:user_id, :resource])

    :ok
  end

  def with_user(query, user_id) do
    from d in query, where: d.user_id == ^user_id
  end

  def and_resource(query, resource) do
    from d in query, where: d.resource == ^resource
  end

  def and_valid(query) do
    from d in query, where: d.invalid == false
  end

  def select_token(query) do
    from d in query, select: d.token
  end

  @spec get(User.id) :: [Device.t]
  def get(user_id) do
    Device
    |> with_user(user_id)
    |> and_valid()
    |> Repo.all
  end

  @spec get_by_token(token) :: Device.t
  def get_by_token(token) do
    Device
    |> where(token: ^token)
    |> Repo.one
  end

  @doc "Return the valid token assigned to the specified user and resource."
  @spec get_token(User.id, User.resource) :: nil | token
  def get_token(user_id, resource) do
    Device
    |> with_user(user_id)
    |> and_resource(resource)
    |> and_valid()
    |> select_token()
    |> Repo.one
  end

  @doc """
  Returns all valid tokens currently assigned to resources belonging to the
  specified user.
  """
  @spec get_all_tokens(User.id) :: [token]
  def get_all_tokens(user_id) do
    Device
    |> with_user(user_id)
    |> and_valid()
    |> select_token()
    |> Repo.all
  end

  @doc "Mark the specified token as invalid"
  @spec invalidate(token, boolean) :: :ok
  def invalidate(token, from_feedback? \\ false) do
    Device
    |> where(token: ^token)
    |> update(set: [invalid: true, feedback: ^from_feedback?])
    |> Repo.update_all([])

    :ok
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
