defmodule Wocky.Tasks.Notify do
  @moduledoc "Notify Slack when deployments are starting and ending"

  alias Slack.Chat

  def start do
    do_notify("Beginning deployment of #{wocky_version_and_target()}...")
  end

  def complete do
    do_notify("Deployment of #{wocky_version_and_target()} complete.")
  end

  defp wocky_version_and_target do
    version = Application.spec(:wocky, :vsn)
    wocky_env = Confex.get_env(:wocky, :wocky_env)
    wocky_inst = Confex.get_env(:wocky, :wocky_inst)
    instance_name = "#{wocky_inst}.#{wocky_env}"
    "Wocky version `#{version}` to `#{instance_name}`"
  end

  defp do_notify(message) do
    Application.ensure_all_started(:slack_ex)
    client = :wocky |> Confex.get_env(:slack_token) |> Slack.client()

    Enum.each(
      ["#development", "#dev-deployments"],
      &send_notification(client, &1, message)
    )
  end

  defp send_notification(client, channel, message) do
    Chat.postMessage(
      client,
      as_user: false,
      channel: channel,
      username: "DevOps Minion",
      icon_url:
        "https://s-media-cache-ak0.pinimg.com/736x/17/4c/a3/" <>
          "174ca39965923045b97862ee34114ea7.jpg",
      text: message
    )
  end
end
