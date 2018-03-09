defmodule Wocky.Account.ClientJWT.Verify do
  @moduledoc "Verifies the claims in a client-generated JWT token."

  use Guardian.Token.Verify

  alias Guardian.Token.Verify
  alias Guardian.Token.Jwt.Verify, as: Base

  @agent_rx ~r/TinyRobot\/(\d\.\d\.\d)(?: \((.*)\))?/

  @doc false
  def verify_claim(_mod, "iss", %{"iss" => iss} = claims, _opts) do
    if client_supported?(iss) do
      {:ok, claims}
    else
      {:error, :invalid_issuer}
    end
  end

  def verify_claim(mod, "aud", %{"aud" => aud} = claims, _opts) do
    audience = mod |> apply(:config, [:audience]) |> to_string()

    if is_nil(aud) || aud == audience do
      {:ok, claims}
    else
      {:error, :invalid_audience}
    end
  end

  def verify_claim(_mod, "iat", %{"iat" => nil} = claims, _opts),
    do: {:ok, claims}

  def verify_claim(mod, "iat", %{"iat" => iat} = claims, _opts) do
    if Verify.time_within_drift?(mod, iat) || iat <= Guardian.timestamp() do
      {:ok, claims}
    else
      {:error, :future_issue_date}
    end
  end

  def verify_claim(_mod, "typ", %{"typ" => typ} = claims, _opts) do
    if Enum.member?(["bypass", "firebase"], typ) do
      {:ok, claims}
    else
      {:error, :unknown_token_type}
    end
  end

  def verify_claim(mod, claim_key, claims, opts),
    do: Base.verify_claim(mod, claim_key, claims, opts)

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
