defmodule WockyAPI.Callbacks.Presence do
  @moduledoc false

  use Dawdle.Handler, only: [Wocky.Presence.PresenceEvent]

  alias WockyAPI.Resolvers.Presence

  def handle_event(%{contact: contact, recipient_id: recipient_id}) do
    Presence.publish_presence(contact, recipient_id)
  end
end
