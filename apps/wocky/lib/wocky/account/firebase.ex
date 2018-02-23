defmodule Wocky.Account.Firebase do
  @moduledoc """
  Module for verifying Firebase JWT tokens
  """

  require Logger

  alias JOSE.JWK
  alias Wocky.Account.FirebaseKeyManager

  @type firebase_id :: binary

  @issuer_prefix "https://securetoken.google.com/"
  @expected_alg "RS256"

  # Verify according to the rules at
  # https://firebase.google.com/docs/auth/admin/verify-id-tokens ...
  # #verify_id_tokens_using_a_third-party_jwt_library
  @spec verify(binary) :: {:ok, firebase_id} | {:error, binary}
  def verify(jwt_binary) do
    with jwt <- Joken.token(jwt_binary),
         headers <- Joken.peek_header(jwt),
         @expected_alg <- headers["alg"],
         key_id <- headers["kid"],
         {:ok, cert} <- FirebaseKeyManager.get_key(key_id),
         {:ok, claims} <- decode_and_verify(jwt, cert),
         user_id when not is_nil(user_id) <- claims["sub"],
         phone when not is_nil(phone) <- claims["phone_number"] do
      {:ok, {user_id, phone}}
    end
    |> make_verify_result()
  end

  defp make_verify_result({:ok, id}), do: {:ok, id}

  defp make_verify_result(failure) do
    Logger.debug(fn -> "Auth failed with error: #{inspect(failure)}" end)
    {:error, "Firebase auth failed"}
  end

  defp decode_and_verify(jwt, cert) do
    project_id = Confex.get_env(:wocky, :firebase_project_id)

    jwt
    |> Joken.with_validation(
      "exp",
      &(&1 > DateTime.utc_now() |> Timex.to_unix()),
      "Expiry is in past"
    )
    |> Joken.with_validation(
      "iat",
      &(&1 <= DateTime.utc_now() |> Timex.to_unix()),
      "Issue time is in future"
    )
    |> Joken.with_validation("aud", &(&1 == project_id), "Invalid audience")
    |> Joken.with_validation(
      "iss",
      &(&1 == @issuer_prefix <> project_id),
      "Invalid issuer"
    )
    |> Joken.with_signer(Joken.rs256(JWK.from_pem(cert)))
    |> Joken.verify!()
  end
end
