defprotocol Wocky.Push.Event do
  @doc "Formats an event into a string for sending in a notification"
  def format(data)
end

defimpl Wocky.Push.Event, for: BitString do
  def format(string), do: string
end
