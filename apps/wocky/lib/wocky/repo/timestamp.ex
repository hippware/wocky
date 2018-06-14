defmodule Wocky.Repo.Timestamp do
  @moduledoc "Timestamp helper functions"

  @format "{ISO:Extended:Z}"
  @regex ~r/\d\d\d\d-\d\d-\d\dT\d\d:\d\d:\d\d\.\d?\d?\d?\d?\d?\d?Z/

  def regex, do: @regex

  @doc "Returns true if the expiry has passed"
  @spec expired?(DateTime.t()) :: boolean
  def expired?(expiry) do
    DateTime.compare(expiry, DateTime.utc_now()) == :lt
  end

  @doc "Parses an ISO-8601 string representation and returns a timestamp"
  @spec from_string(String.t()) :: {:ok, DateTime.t()} | {:error, any}
  def from_string(str) do
    Timex.parse(str, @format)
  end

  @doc "Parses an ISO-8601 string representation and returns a timestamp"
  @spec from_string!(String.t()) :: DateTime.t()
  def from_string!(str) do
    Timex.parse!(str, @format)
  end

  @doc "Returns the ISO-8601 string representation of a timestamp"
  @spec to_string(DateTime.t()) :: String.t()
  def to_string(dt) do
    Timex.format!(dt, @format)
  end

  @doc "Helper function for timestamp sorting"
  @spec less_than_eq?(DateTime.t(), DateTime.t()) :: boolean
  def less_than_eq?(dt1, dt2), do: DateTime.compare(dt1, dt2) != :gt

  @doc "Gives a timestamp shifted from the current UTC time"
  @spec shift(Keyword.t()) :: DateTime.t()
  def shift(modifier) do
    DateTime.utc_now()
    |> Timex.shift(modifier)
  end
end
