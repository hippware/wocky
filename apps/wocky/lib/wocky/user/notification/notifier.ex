defprotocol Wocky.User.Notifier do
  def notify(notification)

  def decode(struct, params)
end
