defprotocol Wocky.Push.Event do
  @doc "Formats an event into a string for sending in a notification"
  def message(data)

  @doc "Returns a URI identifying this specific event"
  def uri(data)
end

defimpl Wocky.Push.Event, for: BitString do
  def message(string), do: string
  def uri(_string), do: ""
end
