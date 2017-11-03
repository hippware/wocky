defmodule Wocky.Token do
  @moduledoc "Handles generation and validation of authentication tokens."

  use Wocky.Repo.Model

  alias Comeonin.Bcrypt
  alias Timex.Duration
  alias Wocky.Repo.Timestamp
  alias Wocky.User
  alias __MODULE__, as: Token

  @foreign_key_type :binary_id
  @primary_key false
  schema "tokens" do
    field :user_id,    :binary_id, null: false, primary_key: true
    field :resource,   :string, null: false, primary_key: true
    field :token_hash, :string, null: false
    field :expires_at, :utc_datetime, null: false

    timestamps()

    belongs_to :user, User, define_field: false
  end

  @type token_hash :: binary
  @type token :: binary
  @type expiry :: DateTime.t
  @type entry :: %Token{
    user_id:    pos_integer,
    resource:   User.resource,
    token_hash: token_hash,
    expires_at: expiry
  }

  @token_bytes 32
  @token_marker "$T$"
  @token_expire Duration.from_weeks(2)
  @assign_fields [:user_id, :resource, :token_hash, :expires_at]

  @doc "Generates a token"
  @spec generate :: token
  def generate do
    string =
      @token_bytes
      |> :crypto.strong_rand_bytes
      |> Base.encode64

    @token_marker <> string
  end

  def changeset(struct, params) do
    struct
    |> cast(params, @assign_fields)
    |> validate_required(@assign_fields)
    |> unique_constraint(:resource, name: :PRIMARY)
  end

  @doc "Generates a token and assigns it to the specified user and resource."
  @spec assign(User.id, User.resource) :: {:ok, {token, expiry}}
  def assign(user_id, resource) do
    token = generate()
    %Token{}
    |> changeset(%{user_id: user_id,
                   resource: resource,
                   token_hash: Bcrypt.hashpwsalt(token),
                   expires_at: expiry()})
    |> Repo.insert!(on_conflict: :replace_all,
                    conflict_target: [:user_id, :resource])
    |> handle_assign_result(token)
  end

  defp expiry, do: Timex.add(DateTime.utc_now, @token_expire)

  defp handle_assign_result(struct, token) do
    {:ok, {token, struct.expires_at}}
  end

  def with_user(query, user_id) do
    from t in query, where: t.user_id == ^user_id
  end

  def and_resource(query, resource) do
    from t in query, where: t.resource == ^resource
  end

  def select_token(query) do
    from t in query, select: t.token
  end

  @doc """
  Returns `true' if a token is valid for the supplied
  user or `false' otherwise.
  """
  @spec valid?(User.id, token) :: boolean
  def valid?(_user_id, nil), do: false
  def valid?(nil, _token), do: false
  def valid?(user_id, token) do
    Token
    |> with_user(user_id)
    |> Repo.all
    |> Enum.to_list
    |> check_token(token)
  rescue
    Ecto.Query.CastError -> false
  end

  # Avoid user-probing timing attack:
  defp check_token([], _) do
    Bcrypt.dummy_checkpw
  end
  defp check_token(records, token) do
    Enum.any?(records, &do_check_token(token, &1))
  end

  defp do_check_token(token, t) do
    Bcrypt.checkpw(token, t.token_hash)
    and
    !Timestamp.expired?(t.expires_at)
  end

  @doc """
  Releases any token currently assigned to the specified user and resource.
  """
  @spec release(User.id, User.resource) :: :ok
  def release(user_id, resource) do
    Token
    |> with_user(user_id)
    |> and_resource(resource)
    |> Repo.delete_all

    :ok
  end

  @doc "Release all tokens currently assigned to the specified user"
  @spec release_all(User.id) :: :ok
  def release_all(user_id) do
    Token
    |> with_user(user_id)
    |> Repo.delete_all

    :ok
  end
end
