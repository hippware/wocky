defmodule Wocky.Presence.ConnectionEvent do
  @moduledoc """
  Event that is fired when the user's connection status
  (connected/disconnected) changes.
  """

  defstruct [:user, :status]
end
