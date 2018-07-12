defmodule Wocky.Account do
  @moduledoc """
  The Account context. Represents the backend bookkeeping required for users
  in the system (authentication, registration, deletion, etc).
  """

  use Elixometer

  alias Wocky.Account.{ClientJWT, Firebase, Register, Token}
  alias Wocky.Repo
  alias Wocky.User

  require Logger

  @type token :: Token.token()

  # ====================================================================
  # User registration

  @doc """
  Creates a new user with a password.
  Used for testing only.
  """
  @spec register(binary, binary, binary) :: {:ok, User.t()} | {:error, any}
  def register(username, password, pass_details) do
    Register.create(%{
      username: username,
      password: password,
      pass_details: pass_details
    })
  end

  # ====================================================================
  # Token management

  @spec generate_token() :: Token.token()
  defdelegate generate_token, to: Token, as: :generate

  @spec assign_token(User.id(), User.resource()) ::
          {:ok, {Token.token(), Token.expiry()}}
  defdelegate assign_token(user_id, resource), to: Token, as: :assign

  @spec release_token(User.id(), User.resource()) :: :ok
  defdelegate release_token(user_id, resource), to: Token, as: :release

  # ====================================================================
  # Authentication

  @type provider :: :token | :bypass | :firebase | :client_jwt

  @doc """
  Authenticates the user using the specified provider and credentials.

  Credentials may be either a binary representing a token or a 2-tuple
  containing an ID and token/password. The provider interprets the credentials,
  so the credential format depends on the provider being used.

  Most providers will create a new account if one does not already exist.

  The following providers are supported:

  ### :token

  Deprecated.

  Authenticates the user using a server-generated token. This provider cannot
  create a new account; it can only authenticate existing accounts.

  The credentials should be the user ID and token.


  ### :bypass

  Bypass authentication only works when it is enabled on the server. This would
  only be for test instances. When it is enabled, authentication always
  succeeds if the phone number begins with the prefix "+1555".

  The credentials for Bypass authentication are the external ID and phone
  number of the user.


  ### :firebase

  Authenticates the user with an access token acquired from Google Firebase.

  The credentials are the access token.


  ### :client_jwt

  This provider wraps another provider and allows for authentication of the
  client and inclusion of metadata. Currently supported wrapped providers
  are Firebase and Bypass. The credentials are a JWT token that was generated
  by the client using the server secret.

  See the Wiki for details:
  https://github.com/hippware/tr-wiki/wiki/Authentication-proposal
  """
  @spec authenticate(provider, binary | {binary, binary}) ::
          {:ok, {User.t(), boolean}} | {:error, binary}
  def authenticate(:token, {user_id, token}) do
    if Token.valid?(user_id, token) do
      update_counter("auth.token.success", 1)
      {:ok, {Repo.get(User, user_id), false}}
    else
      update_counter("auth.token.fail", 1)
      {:error, "Invalid token"}
    end
  end

  def authenticate(:bypass, {external_id, phone_number}) do
    if has_bypass_prefix(phone_number) do
      Register.find_or_create(:bypass, external_id, phone_number)
    else
      provider_error(:bypass)
    end
  end

  def authenticate(:firebase, token) do
    case Firebase.decode_and_verify(token) do
      {:ok, %{"sub" => external_id, "phone_number" => phone_number}} ->
        update_counter("auth.firebase.success", 1)
        Register.find_or_create(:firebase, external_id, phone_number)

      {:error, reason} ->
        update_counter("auth.firebase.fail", 1)
        {:error, error_to_string(reason)}
    end
  end

  def authenticate(:client_jwt, token) do
    case ClientJWT.decode_and_verify(token) do
      {:ok, %{"typ" => "firebase", "sub" => new_token}} ->
        update_counter("auth.jwt.firebase.success", 1)
        authenticate(:firebase, new_token)

      {:ok, %{"typ" => "bypass", "sub" => id, "phone_number" => phone}} ->
        authenticate(:bypass, {id, phone})

      {:ok, _claims} ->
        update_counter("auth.jwt.fail", 1)
        {:error, "Unable to authenticate wrapped entity"}

      {:error, reason} ->
        update_counter("auth.jwt.fail", 1)
        {:error, error_to_string(reason)}
    end
  end

  def authenticate(provider, _creds) do
    update_counter("auth.unknown.fail", 1)
    provider_error(provider)
  end

  defp has_bypass_prefix(phone_number) do
    if Application.get_env(:wocky, :enable_auth_bypass) do
      prefixes = Application.get_env(:wocky, :auth_bypass_prefixes)
      String.starts_with?(phone_number, prefixes)
    end
  end

  defp provider_error(p), do: {:error, "Unsupported provider: #{p}"}

  defp error_to_string(%{message: reason}), do: reason
  defp error_to_string(reason) when is_binary(reason), do: reason
  defp error_to_string(reason) when is_atom(reason), do: to_string(reason)
  defp error_to_string(reason), do: inspect(reason)

  # ====================================================================
  # Account disabling

  @doc """
  Disable a user prior to their eventual deletion so that they cannot
  re-login before the deletion is finalised
  """
  @spec disable_user(User.id()) :: :ok
  def disable_user(user_id) do
    Token.release_all(user_id)
    User.remove_auth_details(user_id)
  end
end
