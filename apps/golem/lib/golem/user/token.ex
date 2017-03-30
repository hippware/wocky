defmodule Golem.User.Token do
  @moduledoc "Handles generation and validation of authentication tokens."

  use Golem.Schema

  import Ecto.Changeset
  import Ecto.Query, only: [from: 2]

  alias Golem.Repo
  alias Golem.Timestamp
  alias Golem.User
  alias __MODULE__, as: Token

  @foreign_key_type :binary_id
  @primary_key false
  schema "user_tokens" do
    field :user_id,    :binary_id, null: false, primary_key: true
    field :resource,   :string, null: false, primary_key: true
    field :token,      :string, null: false
    field :expires_at, :integer, null: false

    timestamps()

    belongs_to :user, User, define_field: false
  end

  @type t :: binary
  @type expiry :: pos_integer
  @type entry :: %Token{
    user_id:    pos_integer,
    resource:   User.resource,
    token:      t,
    expires_at: expiry
  }

  @token_bytes 32
  @token_marker "$T$"
  @token_expire 1_209_600 # 2 weeks

  @doc "Generates a token"
  @spec generate :: t
  def generate do
    string =
      @token_bytes
      |> :crypto.strong_rand_bytes
      |> Base.encode64

    @token_marker <> string
  end

  def changeset(struct, params) do
    struct
    |> cast(params, [:user_id, :resource, :token, :expires_at])
    |> validate_required([:user_id, :resource, :token, :expires_at])
    |> unique_constraint(:resource, name: :PRIMARY)
  end

  @doc "Generates a token and assigns it to the specified user and resource."
  @spec assign(User.id, User.resource) :: {:ok, {t, expiry}} | {:error, any}
  def assign(user_id, resource) do
    %Token{}
    |> changeset(%{
         user_id: user_id,
         resource: resource,
         token: generate(),
         expires_at: Timestamp.now + @token_expire
       })
    |> Repo.insert!(on_conflict: :replace_all)
    |> handle_assign_result
  end

  defp handle_assign_result(struct) do
    {:ok, {struct.token, struct.expires_at}}
  end

  @doc "Return the token assigned to the specified user and resource."
  @spec get_token(User.id, User.resource) :: t | nil
  def get_token(user_id, resource) do
    Repo.one(from t in user_token(user_id, resource), select: t.token)
  end

  defp user_tokens(user_id) do
    from t in Token, where: t.user_id == ^user_id
  end

  defp user_token(id, resource) do
    from t in user_tokens(id), where: t.resource == ^resource
  end

  @doc """
  Returns all tokens currently assigned to resources belonging to the
  specified user.
  """
  @spec get_tokens(User.id) :: [t]
  def get_tokens(user_id) do
    Repo.all(from t in user_tokens(user_id), select: t.token)
  end

  @doc """
  Returns `true' if a token is valid for the supplied
  user or `false' otherwise.
  """
  @spec valid?(User.id, t) :: boolean
  def valid?(user_id, token) do
    user_id
    |> user_tokens()
    |> Repo.all
    |> Enum.any?(&check_token(token, &1))
  end

  defp check_token(token, %Token{token: token} = v), do: !expired?(v.expires_at)
  defp check_token(_, _), do: false

  defp expired?(expiry), do: expiry < Timestamp.now

  @doc """
  Releases any token currently assigned to the specified user and resource.
  """
  @spec release(User.id, User.resource) :: :ok
  def release(user_id, resource) do
    Repo.delete_all(user_token(user_id, resource))
    :ok
  end

  @doc "Release all tokens currently assigned to the specified user"
  @spec release_all(User.id) :: :ok
  def release_all(user_id) do
    Repo.delete_all(user_tokens(user_id))
    :ok
  end
end
