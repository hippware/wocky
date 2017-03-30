defmodule Golem.User do
  @moduledoc ""

  use Golem.Schema

  import Ecto.Changeset
  import Ecto.Query, only: [from: 2]

  alias Golem.ID
  # alias Golem.Index
  alias Golem.Repo
  alias Golem.User.Token
  alias __MODULE__, as: User

  @primary_key {:id, :binary_id, autogenerate: false}
  schema "users" do
    field :username,     :string # User ID (userpart of JID)
    field :server,       :string # User Server (domainpart of JID)
    field :resource,     :string, virtual: true
    field :external_id,  :string # The user ID received from Twitter Digits
    field :handle,       :string # User handle (as seen by other users)
    field :avatar,       :string # ID of file containing user's avatar
    field :first_name,   :string # User's first name
    field :last_name,    :string # User's last name
    field :phone_number, :string # The user's phone number (also from digits)
    field :email,        :string # User's email address
    field :password,     :string # Password hash
    field :pass_details, :string

    timestamps()

    has_many :tokens, Token
  end

  @type id           :: binary
  @type username     :: binary
  @type server       :: binary
  @type resource     :: binary
  @type external_id  :: binary
  @type phone_number :: binary

  @type t :: %User{
    id:             id,
    username:       username,
    server:         server,
    handle:         nil | binary,
    avatar:         nil | binary,
    first_name:     nil | binary,
    last_name:      nil | binary,
    email:          nil | binary,
    external_id:    nil | external_id,
    phone_number:   nil | phone_number,
  }

  @change_fields [:handle, :avatar, :first_name, :last_name, :email,
                  :external_id]


  def changeset(struct, params \\ %{}) do
    struct
    |> cast(params, @change_fields)
    |> validate_format(:email, ~r/@/)
    |> validate_exclusion(:handle, reserved_handles())
    |> unique_constraint(:handle)
    |> unique_constraint(:external_id)
  end

  defp reserved_handles,
    do: Application.get_env(:golem, :reserved_handles, [])

  @doc """
  Creates or updates a user based on the external authentication ID and
  phone number.
  """
  @spec register(binary, binary, binary) ::
    {:ok, {binary, binary, boolean}} | {:error, term}
  def register(server, external_id, phone_number) do
    case Repo.get_by(User, external_id: external_id) do
      nil ->
        username = ID.new
        user = %User{
          id: username,
          username: username,
          server: server,
          external_id: external_id,
          phone_number: phone_number
        }

        user |> changeset |> Repo.insert!
        {:ok, {username, server, true}}

      user ->
        {:ok, {user.username, user.server, false}}
    end
  end

  @doc """
  Update the data on an existing user.
  Fields is a map containing fields to update.
  """
  @spec update(username, map) :: :ok | {:error, term}
  def update(username, fields) do
    # FIXME This is pretty ugly
    case do_update(username, fields) do
      {:ok, _} ->
        do_update_index(username, fields)

      {:error, %Ecto.Changeset{errors: errors}} = error ->
        if Keyword.has_key?(errors, :handle) do
          {:error, :duplicate_handle}
        else
          error
        end
    end
  end

  defp do_update(username, fields) do
    User
    |> Repo.get!(username)
    |> changeset(fields)
    |> Repo.update
  end

  defp do_update_index(_username, _fields) do
    # TODO: Migrate Index to Golem
    # :ok = Index.user_updated(username, fields)
    :ok
  end

  @doc "Removes the user from the database"
  @spec delete(username) :: :ok | no_return
  def delete(username) do
    Repo.delete_all(username_query(username))
    # TODO: Migrate Index to Golem
    # :ok = Index.user_removed(username)
    :ok
  end

  defp username_query(username) do
    from(u in User, where: u.username == ^username)
  end

  @doc """
  Returns a map of all fields for a given user or `nil' if no such
  user exists.
  """
  @spec find(binary) :: t | nil
  def find(username) do
    Repo.get(User, username)
  end

  @doc "Search for a user based on the value of a property"
  @spec find_by(:handle | :phone_number | :external_id, binary) :: t | nil
  def find_by(field, value) do
    Repo.get_by(User, [{field, value}])
  end

  @doc "Convenience function for getting the user's handle"
  @spec get_handle(username) :: binary | nil
  def get_handle(username) do
    Repo.one(from u in username_query(username), select: u.handle)
  end

  @doc "Convenience function for getting the user's phone number"
  @spec get_phone_number(username) :: binary | nil
  def get_phone_number(username) do
    Repo.one(from u in username_query(username), select: u.phone_number)
  end
end
