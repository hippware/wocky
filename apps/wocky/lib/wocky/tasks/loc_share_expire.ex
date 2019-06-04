defmodule Wocky.Tasks.LocShareExpire do
  @moduledoc "Clean up expired location shares"

  require Logger

  alias Wocky.User.{CurrentLocation, LocationShare}

  def run do
    {ls_time, {ls_count, user_ids}} = :timer.tc(&LocationShare.clean_expired/0)

    {cl_time, cl_count} =
      :timer.tc(fn -> CurrentLocation.delete_when_not_shared(user_ids) end)

    Logger.info(
      "Deleted #{ls_count} expired shares in #{ls_time}μs and #{cl_count} stale locations in #{
        cl_time
      }μs"
    )

    :ok
  end
end
