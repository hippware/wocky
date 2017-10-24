defmodule Wocky.DeployNotifier do
  @moduledoc "Notify Slack when a new version is deployed"

  alias Slack.Chat

  @username "DevOps Minion"
  @icon_url "https://s-media-cache-ak0.pinimg.com/736x/17/4c/a3/" <>
            "174ca39965923045b97862ee34114ea7.jpg"

  def notify do
    client = :wocky |> Confex.get_env(:slack_token) |> Slack.client

    version = Application.spec(:wocky, :vsn)
    wocky_env = Confex.get_env(:wocky, :wocky_env)
    wocky_inst = Confex.get_env(:wocky, :wocky_inst)
    instance_name = "#{wocky_inst}.#{wocky_env}"
    message = "Wocky release `#{version}` deployed to #{instance_name}"

    send_notification(client, "#development", message)
    send_notification(client, "#dev-deployments", message)

    :ok
  end

  defp send_notification(client, channel, message) do
    Chat.postMessage(client, [
      as_user: false,
      channel: channel,
      username: @username,
      icon_url: @icon_url,
      text: message
    ])
  end
end
