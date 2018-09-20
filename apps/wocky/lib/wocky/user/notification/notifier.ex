defprotocol Wocky.User.Notifier do
  alias Wocky.User.Notification

  @spec notify(Notification.t()) :: {:ok, Notification.base()} | {:error, any()}
  def notify(notification)

  @spec decode(Notification.t(), Notification.base()) :: Notification.t()
  def decode(struct, params)
end
