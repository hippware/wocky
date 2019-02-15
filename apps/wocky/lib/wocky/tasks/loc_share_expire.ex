defmodule Wocky.Tasks.LocShareExpire do
  @moduledoc "Clean up expired location shares"

  require Logger

  alias Wocky.User.LocationShare

  def run do
    {time, {count, nil}} = :timer.tc(&LocationShare.clean_expired/0)

    Logger.info("Deleted #{count} expired shares in #{time}ms")
    :init.stop()
  end
end
