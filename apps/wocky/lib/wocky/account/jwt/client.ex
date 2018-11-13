defmodule Wocky.Account.JWT.Client do
  @moduledoc """
  Validates the client-generated JWT used to wrap OAuth 2 tokens.
  See https://github.com/hippware/tr-wiki/wiki/Authentication-proposal
  """
  use Guardian,
    otp_app: :wocky,
    issuer: "TinyRobot/0.0.0 (Wocky)",
    secret_key:
      "0xszZmLxKWdYjvjXOxchnV+ttjVYkU1ieymigubkJZ9dqjnl7WPYLYqLhvC10TaH",
    token_verify_module: Wocky.Account.JWT.Verify

  alias Wocky.Account.JWT.Firebase
  alias Wocky.Account.Register
  alias Wocky.User

  @audience "Wocky"
  @agent_rx ~r/TinyRobot\/(\d+\.\d+\.\d+)(?: \((.*)\))?/

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

  def verify_claims(claims, _opts) do
    with :ok <- verify_aud(claims["aud"]),
         :ok <- verify_typ(claims["typ"]),
         :ok <- verify_iss(claims["iss"]) do
      {:ok, claims}
    end
  end

  defp verify_aud(aud) do
    if is_nil(aud) || aud == @audience do
      :ok
    else
      {:error, :invalid_audience}
    end
  end

  defp verify_typ(typ) do
    if Enum.member?(["bypass", "firebase"], typ) do
      :ok
    else
      {:error, :unknown_token_type}
    end
  end

  defp verify_iss(iss) do
    if client_supported?(iss) do
      :ok
    else
      {:error, :invalid_issuer}
    end
  end

  defp client_supported?(nil), do: false

  defp client_supported?(agent_str) do
    case parse_agent(agent_str) do
      {:ok, version, attrs} -> client_supported?(version, attrs)
      {:error, _} -> false
    end
  end

  defp parse_agent(agent_str) do
    case Regex.run(@agent_rx, agent_str) do
      nil -> {:error, :unknown_client}
      [_, version] -> {:ok, version, []}
      [_, version, attrs] -> {:ok, version, parse_attrs(attrs)}
    end
  end

  defp parse_attrs(""), do: []

  defp parse_attrs(attrs) do
    attrs
    |> String.split(";", trim: true)
    |> Enum.each(&String.trim/1)
  end

  defp client_supported?(_version, _attrs) do
    # Always return true for now
    true
  end
end
