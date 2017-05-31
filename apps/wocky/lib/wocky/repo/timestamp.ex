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
  @spec to_string(t | DateTime.t) :: binary
  def to_string(ts) when is_integer(ts) do
    ts
    |> Timex.from_unix
    |> Timex.format!("{ISO:Extended}")
  end
  def to_string(dt) do
    Timex.format!(dt, "{ISO:Extended}")
  end

  @doc "Helper function for timestamp sorting"
  @spec less_than_eq?(DateTime.t, DateTime.t) :: boolean
  def less_than_eq?(dt1, dt2), do: DateTime.compare(dt1, dt2) != :gt

end
