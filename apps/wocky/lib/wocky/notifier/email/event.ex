defprotocol Wocky.Notifier.Email.Event do
  @fallback_to_any true

  @doc "Whether or not to notify via email"
  @spec notify?(t()) :: boolean()
  def notify?(event)

  @doc "Send the event notification via email"
  @spec send(t()) :: :ok
  def send(event)

  @doc "Indicates that the event should be sent even if there is a block between users"
  @spec ignore_block?(t()) :: boolean()
  def ignore_block?(event)
end

defimpl Wocky.Notifier.Email.Event, for: Any do
  def notify?(_event), do: false

  def send(_event), do: :ok

  def ignore_block?(_event), do: false
end
