defmodule Wocky.Timestamp do
  @moduledoc "Timestamps for use in the database"

  def now, do: DateTime.to_unix(DateTime.utc_now)
end
