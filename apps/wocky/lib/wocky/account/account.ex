defmodule Wocky.Account do
  @moduledoc """
  The Account context. Represents the backend bookkeeping required for users
  in the system (authentication, registration, deletion, etc).
  """

  import Ecto.Query, warn: false
  import Ecto.Changeset

  alias Wocky.Account.{Firebase, Token}
  alias Wocky.InitialContact
  alias Wocky.RosterItem
  alias Wocky.HomeStream
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

  @spec authenticate_with_digits(binary, binary, binary) ::
          {:ok, User.t()} | {:error, any}
  def authenticate_with_digits(server, external_id, phone_number) do
    if has_bypass_prefix(phone_number) do
      on_authenticated(server, "digits", external_id, phone_number)
    else
      {:error, :invalid_user}
    end
  end

  defp has_bypass_prefix(phone_number) do
    if Application.get_env(:wocky, :enable_auth_bypass) do
      prefixes = Application.get_env(:wocky, :auth_bypass_prefixes)
      String.starts_with?(phone_number, prefixes)
    end
  end

  @spec authenticate_with_firebase(binary, binary) ::
          {:ok, User.t()} | {:error, any}
  def authenticate_with_firebase(server, jwt) do
    case Firebase.verify(jwt) do
      {:ok, {external_id, phone_number}} ->
        on_authenticated(server, "firebase", external_id, phone_number)

      {:error, _} = error ->
        error
    end
  end

  @doc false
  def on_authenticated(server, provider, external_id, phone_number) do
    on_authenticated(server, provider, external_id, phone_number, 0)
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
  defp on_authenticated(_, _, _, _, @max_register_retries) do
    raise "Exceeded maximum registration retries"
  end

  defp on_authenticated(server, provider, external_id, phone_number, retries) do
    case Repo.get_by(User, external_id: external_id, provider: provider) do
      nil ->
        case Repo.get_by(User, phone_number: phone_number) do
          nil ->
            register_new(server, provider, external_id, phone_number, retries)

          orig_user ->
            {:ok, user} = User.update(orig_user, %{
              provider: provider,
              external_id: external_id,
              phone_number: phone_number
            })

            {:ok, {user, false}}
        end

      user ->
        {:ok, {user, false}}
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
        prepopulate_user(user.id)
        {:ok, {user, true}}

      {:error, e} ->
        Logger.debug(fn -> "registration failed with error: #{inspect(e)}" end)

        on_authenticated(
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

  defp prepopulate_user(user_id) do
    set_initial_contacts(user_id)
    HomeStream.prepopulate(user_id)
  end

  defp set_initial_contacts(user_id) do
    InitialContact.get()
    |> Enum.each(&set_initial_contact(user_id, &1))
  end

  defp set_initial_contact(user_id, %{user: user, type: :followee}) do
    set_initial_contact(user_id, user, :to, :from)
  end

  defp set_initial_contact(user_id, %{user: user, type: :follower}) do
    set_initial_contact(user_id, user, :from, :to)
  end

  defp set_initial_contact(user_id, %{user: user, type: :friend}) do
    set_initial_contact(user_id, user, :both, :both)
  end

  defp set_initial_contact(user_id, followee, usub, fsub) do
    user_contact = %{
      user_id: user_id,
      contact_id: followee.id,
      name: followee.handle,
      ask: :none,
      subscription: usub,
      groups: ["__welcome__", "__new__"]
    }

    init_contact = %{
      user_id: followee.id,
      contact_id: user_id,
      name: "",
      ask: :none,
      subscription: fsub,
      groups: ["__welcomed__", "__new__"]
    }

    RosterItem.put(user_contact)
    RosterItem.put(init_contact)
  end
end
