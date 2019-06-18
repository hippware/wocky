defmodule Wocky.Tasks.LocShareExpire do
  @moduledoc "Clean up expired location shares"

  require Logger

  alias Wocky.Location

  def run do
    {ls_time, {ls_count, user_ids}} =
      :timer.tc(&Location.clean_expired_shares/0)

    {cl_time, cl_count} =
      :timer.tc(fn -> Location.clean_current_locations(user_ids) end)

    if ls_count > 0 || cl_count > 0 do
      Logger.info("""
      Deleted #{ls_count} expired shares in #{ls_time}μs and #{cl_count} \
      stale locations in #{cl_time}μs
      """)
    end

    :ok
  end
end
