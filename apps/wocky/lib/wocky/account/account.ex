defmodule Wocky.Account do
  @moduledoc """
  The Account context. Represents the backend bookkeeping required for users
  in the system (authentication, registration, deletion, etc).
  """

  import Ecto.Query, warn: false
  import Ecto.Changeset

  alias Wocky.Account.Token
  alias Wocky.Repo
  alias Wocky.Repo.ID
  alias Wocky.User

  require Logger

  @type token :: Token.token()

  @max_register_retries 5

  @changeset_fields [
    :username,
    :server,
    :provider,
    :external_id,
    :phone_number,
    :password,
    :pass_details
  ]

  # ====================================================================
  # User registration

  @doc """
  Creates a new user with a password.
  Used for testing only.
  """
  @spec register(binary, binary, binary, binary) ::
          {:ok, User.t()} | {:error, any}
  def register(username, server, password, pass_details) do
    %{
      username: username,
      server: server,
      provider: "local",
      external_id: username,
      password: password,
      pass_details: pass_details
    }
    |> changeset()
    |> Repo.insert()
  end

  @doc """
  Creates or updates a user based on the external authentication ID and
  phone number.
  """
  @spec register_external(binary, binary, binary, binary) ::
          {:ok, {binary, binary, boolean}} | no_return
  def register_external(server, provider, external_id, phone_number) do
    do_register(server, provider, external_id, phone_number, 0)
  end

  # There's scope for a race condition here. Since the database uses "read
  # committed"
  # (see https://www.postgresql.org/docs/current/static/transaction-iso.html)
  # we can't SELECT then INSERT and assume something hasn't been inserted
  # in the interim, even in a transaction. Thus we implement a retry system
  # here - if the user was inserted after the SELECT, the next time around it
  # should work fine (unless it gets deleted in the interim in which case
  # what the heck is even going on?). Either way, if we fail after 5 retries
  # there's something seriously weird going on and raising an exception
  # is a pretty reasonable response.
  defp do_register(_, _, _, _, @max_register_retries) do
    raise "Exceeded maximum registration retries"
  end

  defp do_register(
         server,
         provider,
         external_id,
         phone_number,
         retries
       ) do
    case Repo.get_by(User, external_id: external_id, provider: provider) do
      nil ->
        case Repo.get_by(User, phone_number: phone_number) do
          nil ->
            register_new(server, provider, external_id, phone_number, retries)

          user ->
            User.update(user.id, %{
              provider: provider,
              external_id: external_id,
              phone_number: phone_number
            })

            {:ok, {user.id, user.server, false}}
        end

      user ->
        {:ok, {user.id, user.server, false}}
    end
  end

  defp register_new(server, provider, external_id, phone_number, retries) do
    user_data = %{
      username: ID.new(),
      server: server,
      provider: provider,
      external_id: external_id,
      phone_number: phone_number
    }

    case user_data |> changeset() |> Repo.insert() do
      {:ok, user} ->
        {:ok, {user.id, user.server, true}}

      {:error, e} ->
        Logger.debug(fn -> "registration failed with error: #{inspect(e)}" end)

        do_register(
          server,
          provider,
          external_id,
          phone_number,
          retries + 1
        )
    end
  end

  @doc false
  def changeset(attrs) do
    %User{}
    |> cast(attrs, @changeset_fields)
    |> validate_required([:username, :server, :provider, :external_id])
    |> validate_format(:phone_number, ~r//)
    |> validate_change(:username, &validate_username/2)
    |> put_change(:id, attrs[:username])
    |> unique_constraint(:external_id)
  end

  defp validate_username(:username, username) do
    if ID.valid?(username) do
      []
    else
      [username: "not a valid UUID"]
    end
  end

  # ====================================================================
  # Token management

  @spec generate_token() :: Token.token()
  def generate_token, do: Token.generate()

  @spec assign_token(User.id(), User.resource()) ::
          {:ok, {Token.token(), Token.expiry()}}
  def assign_token(user_id, resource), do: Token.assign(user_id, resource)

  @spec release_token(User.id(), User.resource()) :: :ok
  def release_token(user_id, resource), do: Token.release(user_id, resource)

  # ====================================================================
  # Authentication

  @spec authenticate_with_token(User.id(), Token.token()) ::
          {:ok, User.t()} | {:error, any}
  def authenticate_with_token(user_id, token) do
    if Token.valid?(user_id, token) do
      {:ok, Repo.get(User, user_id)}
    else
      {:error, :invalid_token}
    end
  end
end
