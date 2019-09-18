defprotocol Wocky.Notifier.InBand.Event do
  @fallback_to_any true

  @doc "Should the event be sent via in-band notification?"
  def notify?(event)

  @doc "Returns an atom identifying this event type"
  def event_type(event)

  @doc "Returns the map fields required for this event type"
  def required_fields(event)

  @doc "Transforms an event into a map for sending in a notification"
  def transform(event)

  @doc "Indicates that the event should be sent even if there is a block between users"
  def ignore_block?(event)
end

defimpl Wocky.Notifier.InBand.Event, for: Any do
  def notify?(_event), do: false

  def event_type(_event), do: nil

  def required_fields(_event), do: []

  def transform(_event), do: %{}

  def ignore_block?(_event), do: false
end
