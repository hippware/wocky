defmodule Wocky.Notification.NullHandler do
  @behaviour :wocky_notification_handler

  def register(_user, device) do
    {:ok, device}
  end

  def notify(_endpoint, _from, _body) do
    :ok
  end
end
