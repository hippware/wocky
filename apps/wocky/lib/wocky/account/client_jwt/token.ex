defmodule Wocky.Account.ClientJWT.Token do
  @moduledoc "Implements Guardian's Token behavior for client-generated JWTs"

  @behaviour Guardian.Token

  import Guardian, only: [stringify_keys: 1, returning_tuple: 1]

  alias Guardian.Token.Jwt

  defdelegate peek(mod, token), to: Jwt

  defdelegate create_token(mod, claims, options \\ []), to: Jwt

  defdelegate decode_token(mod, token, options \\ []), to: Jwt

  defdelegate verify_claims(mod, claims, options), to: Jwt

  defdelegate revoke(mod, claims, token, options), to: Jwt

  defdelegate refresh(mod, old_token, options), to: Jwt

  defdelegate exchange(mod, old_token, from_type, to_type, options), to: Jwt

  # The default implementation of token_id relies on the UUID module which
  # conflicts with the Erlang uuid library that we are obliged to depend on
  # because MongooseIM uses it.
  def token_id do
    10
    |> :crypto.strong_rand_bytes()
    |> Base.encode16()
  end

  def build_claims(mod, resource, sub, claims \\ %{}, options \\ []) do
    claims =
      claims
      |> stringify_keys()
      |> set_jti()
      |> set_iat()
      |> set_iss(mod, options)
      |> set_aud(mod, options)
      |> set_type(mod, resource, options)
      |> set_sub(mod, sub, options)
      |> set_ttl(mod, options)

    {:ok, claims}
  end

  defp set_jti(claims), do: Map.put(claims, "jti", token_id())

  defp set_iat(claims) do
    ts = Guardian.timestamp()
    claims |> Map.put("iat", ts) |> Map.put("nbf", ts - 1)
  end

  defp set_iss(claims, mod, _opts) do
    issuer = mod |> apply(:config, [:issuer]) |> to_string()
    Map.put(claims, "iss", issuer)
  end

  defp set_aud(claims, mod, _opts) do
    audience = mod |> apply(:config, [:audience]) |> to_string()
    Map.put(claims, "aud", audience)
  end

  defp set_type(claims, mod, resource, _opts) do
    {:ok, typ} = returning_tuple({mod, :type_for_token, [resource, claims]})
    Map.put(claims, "typ", to_string(typ))
  end

  defp set_sub(claims, _mod, subject, _opts),
    do: Map.put(claims, "sub", subject)

  defp set_ttl(%{"iat" => iat} = claims, _mod, _opts),
    do: Map.put(claims, "exp", iat + 60 * 60)
end
