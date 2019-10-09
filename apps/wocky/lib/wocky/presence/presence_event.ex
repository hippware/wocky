defmodule Wocky.Presence.PresenceEvent do
  @moduledoc """
  Event that is fired when the user's presence status (online/offline) changes.
  """

  defstruct [:contact, :recipient_id]
end
