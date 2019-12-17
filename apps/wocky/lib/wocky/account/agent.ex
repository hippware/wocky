defmodule Wocky.Account.Agent do
  @moduledoc """
  Utility module for handling user agent strings.
  """

  @spec supported?(String.t() | nil) :: boolean()
  def supported?(nil), do: false

  def supported?(agent_str) do
    case parse(agent_str) do
      {:ok, version, attrs} -> supported?(version, attrs)
      {:error, _} -> false
    end
  end

  @spec parse(String.t()) :: {:ok, String.t(), Keyword.t()} | {:error, atom()}
  def parse(agent_str) do
    case Regex.run(agent_re(), agent_str) do
      nil -> {:error, :unknown_client}
      [_, _, version] -> {:ok, version, []}
      [_, _, version, attrs] -> {:ok, version, parse_attrs(attrs)}
    end
  end

  defp parse_attrs(""), do: []

  defp parse_attrs(attrs) do
    attrs
    |> String.split(";", trim: true)
    |> Enum.map(&String.trim/1)
  end

  defp supported?(_version, _attrs) do
    # Always return true for now
    true
  end

  defp agent_re do
    agents =
      :wocky
      |> Confex.get_env(:permitted_agents)
      |> Enum.join("|")

    ~r/(#{agents})\/((?:\d+\.?)+)(?: \((.*)\))?/
  end
end
