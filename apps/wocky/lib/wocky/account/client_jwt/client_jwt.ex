defmodule Wocky.Account.ClientJWT do
  @moduledoc """
  Validates the client-generated JWT used to wrap OAuth 2 tokens.
  See https://github.com/hippware/tr-wiki/wiki/Authentication-proposal
  """
  @audience "Wocky"

  use Guardian,
    otp_app: :wocky,
    issuer: "TinyRobot/0.0.0 (Wocky)",
    audience: @audience,
    secret_key:
      "0xszZmLxKWdYjvjXOxchnV+ttjVYkU1ieymigubkJZ9dqjnl7WPYLYqLhvC10TaH",
    token_verify_module: Wocky.Account.ClientJWT.Verify

  alias Wocky.Account.{Firebase, Register}
  alias Wocky.User

  def subject_for_token(%User{} = user, _claims) do
    {:ok, Register.get_external_id(user, "bypass")}
  end

  def subject_for_token(token, _claims) when is_binary(token) do
    {:ok, token}
  end

  def subject_for_token(_, _) do
    {:error, :unknown_resource}
  end

  def resource_from_claims(%{"typ" => "firebase", "sub" => token}) do
    {:ok, user, _claims} = Firebase.resource_from_token(token)
    {:ok, user}
  end

  def resource_from_claims(%{"typ" => "bypass"} = claims) do
    Register.find(:bypass, claims["sub"], claims["phone_number"])
  end

  def resource_from_claims(_claims) do
    {:error, :not_possible}
  end

  def build_claims(claims, %User{} = user, _opts) do
    claims =
      claims
      |> Map.put("phone_number", user.phone_number)
      |> Map.put("typ", "bypass")
      |> Map.put("aud", @audience)

    {:ok, claims}
  end

  def build_claims(claims, token, _opts) when is_binary(token) do
    claims =
      claims
      |> Map.put("typ", "firebase")
      |> Map.put("aud", @audience)

    {:ok, claims}
  end

  def build_claims(claims, _resource, _opts), do: {:ok, claims}
end
