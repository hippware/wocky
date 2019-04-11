defprotocol Wocky.Notifier.Mailer.Event do
  @fallback_to_any true

  def notify?(event)
end

defimpl Wocky.Notifier.Mailer.Event, for: Any do
  def notify?(_event), do: false
end
