defmodule Wocky.Notification.NullHandler do
  @behaviour :wocky_notification_handler

  def register(_user, _platform, device) do
    {:ok, device}
  end

  def notify(_endpoint, _from, _body) do
    :ok
  end
end
