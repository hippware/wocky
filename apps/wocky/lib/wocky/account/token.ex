defmodule Wocky.Account.Token do
  @moduledoc "Handles generation and validation of authentication tokens."

  use Wocky.Repo.Schema

  import Ecto.Query

  alias Comeonin.Bcrypt
  alias Timex.Duration
  alias Wocky.Repo
  alias Wocky.Repo.Timestamp
  alias Wocky.User

  @foreign_key_type :binary_id
  @primary_key false
  schema "tokens" do
    field :user_id, :binary_id, null: false, primary_key: true
    field :device, :string, null: false, primary_key: true
    field :token_hash, :string, null: false
    field :expires_at, :utc_datetime_usec, null: false

    timestamps()

    belongs_to :user, User, define_field: false
  end

  @type token_hash :: binary
  @type token :: binary
  @type expiry :: DateTime.t()
  @type entry :: %Token{
          user_id: pos_integer,
          device: User.device(),
          token_hash: token_hash,
          expires_at: expiry
        }

  @token_bytes 32
  @token_marker "$T$"
  @assign_fields [:user_id, :device, :token_hash, :expires_at]

  @doc "Generates a token"
  @spec generate :: token
  def generate do
    string =
      @token_bytes
      |> :crypto.strong_rand_bytes()
      |> Base.encode64()

    @token_marker <> string
  end

  def changeset(struct, params) do
    struct
    |> cast(params, @assign_fields)
    |> validate_required(@assign_fields)
    |> unique_constraint(:device, name: :PRIMARY)
  end

  @doc "Generates a token and assigns it to the specified user and device."
  @spec assign(User.id(), User.device()) :: {:ok, {token, expiry}}
  def assign(user_id, device) do
    token = generate()

    %Token{}
    |> changeset(%{
      user_id: user_id,
      device: device,
      token_hash: Bcrypt.hashpwsalt(token),
      expires_at: expiry()
    })
    |> Repo.insert!(
      on_conflict: :replace_all,
      conflict_target: [:user_id, :device]
    )
    |> handle_assign_result(token)
  end

  defp expiry, do: Timex.add(DateTime.utc_now(), expiry_duration())

  defp expiry_duration, do: Duration.from_days(expiry_days())

  defp expiry_days, do: Confex.get_env(:wocky, :token_expiry_days)

  defp handle_assign_result(struct, token) do
    {:ok, {token, struct.expires_at}}
  end

  defp with_user(query, user_id) do
    from t in query, where: t.user_id == ^user_id
  end

  defp and_device(query, device) do
    from t in query, where: t.device == ^device
  end

  @doc """
  Returns `true' if a token is valid for the supplied
  user or `false' otherwise.
  """
  @spec valid?(User.id(), token) :: boolean
  def valid?(_user_id, nil), do: false
  def valid?(nil, _token), do: false

  def valid?(user_id, token) do
    Token
    |> with_user(user_id)
    |> Repo.all()
    |> Enum.to_list()
    |> check_token(token)
  rescue
    Ecto.Query.CastError -> false
  end

  # Avoid user-probing timing attack:
  defp check_token([], _) do
    Bcrypt.dummy_checkpw()
  end

  defp check_token(records, token) do
    Enum.any?(records, &do_check_token(token, &1))
  end

  defp do_check_token(token, t) do
    Bcrypt.checkpw(token, t.token_hash) and !Timestamp.expired?(t.expires_at)
  end

  @doc """
  Releases any token currently assigned to the specified user and device.
  """
  @spec release(User.id(), User.device()) :: :ok
  def release(user_id, device) do
    Token
    |> with_user(user_id)
    |> and_device(device)
    |> Repo.delete_all()

    :ok
  end

  @doc "Release all tokens currently assigned to the specified user"
  @spec release_all(User.id()) :: :ok
  def release_all(user_id) do
    Token
    |> with_user(user_id)
    |> Repo.delete_all()

    :ok
  end
end
