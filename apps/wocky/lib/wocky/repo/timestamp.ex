defmodule Wocky.Repo.Timestamp do
  @moduledoc "Timestamp helper functions"

  @format "{ISO:Extended:Z}"

  @doc "Parses an ISO-8601 string representation and returns a timestamp"
  @spec from_string!(String.t()) :: DateTime.t()
  def from_string!(str) do
    Timex.parse!(str, @format)
  end

  @doc "Returns the ISO-8601 string representation of a timestamp"
  @spec to_string!(DateTime.t()) :: String.t()
  def to_string!(dt) do
    Timex.format!(dt, @format)
  end

  @doc "Gives a timestamp shifted from the current UTC time"
  @spec shift(Keyword.t()) :: DateTime.t()
  def shift(modifier) do
    DateTime.utc_now()
    |> Timex.shift(modifier)
  end
end
