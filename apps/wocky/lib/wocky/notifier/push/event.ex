defprotocol Wocky.Notifier.Push.Event do
  @fallback_to_any true

  @doc "Should this event result in a push notification?"
  def notify?(event)

  @doc "Who should the push notification be sent to"
  def recipient(event)

  @doc "Formats an event into a string for sending in a notification"
  def message(event)

  @doc "Returns a URI identifying this specific event"
  def uri(event)

  @doc "Indicates that the event should be sent even if there is a block between users"
  def ignore_block?(event)

  @doc "Returns a keyword list of optional parameters"
  def opts(event)
end

defimpl Wocky.Notifier.Push.Event, for: Any do
  def notify?(_event), do: false

  def recipient(_event), do: nil

  def message(_event), do: ""

  def uri(_event), do: ""

  def ignore_block?(_event), do: false

  def opts(_event), do: []
end

defimpl Wocky.Notifier.Push.Event, for: BitString do
  def notify?(_event), do: false

  def recipient(_event), do: nil

  def message(string), do: string

  def uri(_event), do: ""

  def ignore_block?(_event), do: false

  def opts(_event), do: []
end
