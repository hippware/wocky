defmodule Wocky.Notification.AWSHandler do
  alias ExAws.SNS

  @behaviour :wocky_notification_handler

  @topic_arn "arn:aws:sns:us-east-1:773488857071:wocky"
  @subject "Message Received"

  def notify(_from, _to, body) do
    {:ok, _response} =
      body
      |> SNS.publish(%{subject: @subject, topic_arn: @topic_arn})
      |> ExAws.request

    :ok
  end
end
