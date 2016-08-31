defmodule Wocky.Notification.NullHandler do
  @behaviour :wocky_notification_handler

  def notify(_from, _to, _body) do
    :ok
  end
end
