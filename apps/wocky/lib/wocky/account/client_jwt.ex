defmodule Wocky.Account.ClientJWT do
  @moduledoc """
  Validates the client-generated JWT used to wrap OAuth 2 tokens.
  See https://github.com/hippware/tr-wiki/wiki/Authentication-proposal
  """

  require Logger

  @agent_rx ~r/TinyRobot\/(\d\.\d\.\d)(?: \((.*)\))?/
  @token_secret "secret"

  @doc "Verifies the token and calls the appropriate provider"
  @spec verify(binary) :: {:ok, {binary, binary, binary}} | {:error, any}
  def verify(jwt_binary) do
    with jwt <- Joken.token(jwt_binary),
         {:ok, claims} <- decode_and_verify(jwt),
         provider when not is_nil(provider) <- claims["prv"],
         token when not is_nil(token) <- claims["tkn"] do
      {:ok, {provider, token}}
    end
    |> make_verify_result()
  end

  defp make_verify_result({:ok, id}), do: {:ok, id}

  defp make_verify_result(failure) do
    Logger.debug(fn -> "Auth failed with error: #{inspect(failure)}" end)
    {:error, "Client JWT verification failed"}
  end

  defp decode_and_verify(jwt) do
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
    |> Joken.with_validation(
      "nbf",
      &(&1 <= DateTime.utc_now() |> Timex.to_unix()),
      "Not before time is in the future"
    )
    |> Joken.with_validation(
      "aud",
      &(is_nil(&1) || &1 == "Wocky"),
      "Invalid audience"
    )
    |> Joken.with_validation(
      "iss",
      &client_supported?/1,
      "Invalid or unknown client"
    )
    |> Joken.with_signer(Joken.hs512(@token_secret))
    |> Joken.verify!()
  end

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
