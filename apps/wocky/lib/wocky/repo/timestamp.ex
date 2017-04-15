defmodule Wocky.Repo.Timestamp do
  @moduledoc "Timestamps for use in the database"

  @type t :: integer

  @doc "Returns a Unix timestamp"
  @spec now :: t
  def now, do: DateTime.to_unix(DateTime.utc_now)

  @doc "Returns true if the expiry has passed"
  @spec expired?(t) :: boolean
  def expired?(expiry), do: expiry < now()

  @doc "Returns the ISO-8601 string representation of a timestamp"
  @spec to_string(t) :: binary
  def to_string(ts) do
    ts
    |> Timex.from_unix
    |> Timex.format!("{ISO:Extended}")
  end
end
