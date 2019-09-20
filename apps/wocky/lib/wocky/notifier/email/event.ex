defprotocol Wocky.Notifier.Email.Event do
  @fallback_to_any true

  @doc "Whether or not to notify via email"
  def notify?(event)

  @doc "Send the event notification via email"
  def send(event)
end

defimpl Wocky.Notifier.Email.Event, for: Any do
  def notify?(_event), do: false

  def send(_event), do: :ok
end
