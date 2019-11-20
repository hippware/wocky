defprotocol Wocky.Notifier.Push.Event do
  @fallback_to_any true

  @doc "Should this event result in a push notification?"
  @spec notify?(t()) :: boolean()
  def notify?(event)

  @doc "Who should the push notification be sent to"
  @spec recipient(t()) :: Wocky.Account.User.t() | nil
  def recipient(event)

  @doc "Formats an event into a string for sending in a notification"
  @spec message(t()) :: String.t()
  def message(event)

  @doc "Returns a URI identifying this specific event"
  @spec uri(t()) :: String.t() | nil
  def uri(event)

  @doc "Indicates that the event should be sent even if there is a block between users"
  @spec ignore_block?(t()) :: boolean()
  def ignore_block?(event)

  @doc "Returns a keyword list of optional parameters"
  @spec opts(t()) :: Keyword.t()
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
