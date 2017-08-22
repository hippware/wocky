defmodule Wocky.Auth.Firebase do
  @moduledoc """
  Module for verifying Firebase JWT tokens
  """

  require Logger

  alias JOSE.JWK
  alias Wocky.Auth.FirebaseKeyManager

  @type firebase_id :: binary

  @issuer_prefix "https://securetoken.google.com/"
  @expected_alg "RS256"

  # Verify according to the rules at
  # https://firebase.google.com/docs/auth/admin/verify-id-tokens ...
  # #verify_id_tokens_using_a_third-party_jwt_library
  @spec verify(binary) :: {:ok, {firebase_id, binary}} | {:error, term}
  def verify(jwt_binary) do
    result =
      with jwt <- Joken.token(jwt_binary),
           headers <- Joken.peek_header(jwt),
           @expected_alg <- headers["alg"],
           key_id <- headers["kid"],
           {:ok, cert} <- FirebaseKeyManager.get_key(key_id),
           {:ok, claims} <- decode_and_verify(jwt, cert),
           user_id <- claims["sub"]
      do
        {:ok, user_id}
      end
    case result do
      {:ok, id} ->
        {:ok, id}
      fail ->
        Logger.debug("Auth failed with error: #{inspect fail}")
        {:error, "Firebase auth failed"}
    end
  end

  defp decode_and_verify(jwt, cert) do
    project_id = Confex.get(:wocky, :firebase_project_id)

    jwt
    |> Joken.with_validation("exp", &(&1 > DateTime.utc_now |> Timex.to_unix),
                             "Expiry is in past")
    |> Joken.with_validation("iat", &(&1 <= DateTime.utc_now |> Timex.to_unix),
                             "Issue time is in future")
    |> Joken.with_validation("aud", &(&1 == project_id), "Invalid audience")
    |> Joken.with_validation("iss", &(&1 == @issuer_prefix <> project_id),
                             "Invalid issuer")
    |> Joken.with_signer(Joken.rs256(JWK.from_pem(cert)))
    |> Joken.verify!
  end
end
