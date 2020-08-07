defmodule Wocky.Tasks.Notify do
  @moduledoc "Notify Slack when deployments are starting and ending"

  alias Slack.Chat

  @spec start :: :ok
  def start do
    do_notify("Beginning deployment of #{wocky_version_and_target()}...")
  end

  @spec complete(String.t(), String.t()) :: :ok
  def complete(result, extra \\ "") do
    msg! = "Deployment of Wocky to #{instance_name()} "

    msg! =
      if result == "failed" do
        msg! <> "FAILED!"
      else
        msg! <> "completed successfully."
      end

    msg! =
      if extra != "" do
        msg! <> "\n" <> extra
      else
        msg!
      end

    do_notify(msg!)
  end

  @spec notify(String.t()) :: :ok
  def notify(msg) do
    do_notify(msg)
  end

  defp wocky_version_and_target do
    version = Application.spec(:wocky, :vsn)
    "Wocky version `#{version}` to #{instance_name()}"
  end

  defp instance_name do
    wocky_env = Confex.get_env(:wocky, :wocky_env)
    wocky_inst = Confex.get_env(:wocky, :wocky_inst)
    "`#{wocky_inst}.#{wocky_env}`"
  end

  defp do_notify(message) do
    for app <- [:slack_ex, :ex_aws] do
      {:ok, _} = Application.ensure_all_started(app)
    end

    client = :wocky |> Confex.get_env(:slack_token) |> Slack.client()

    :wocky
    |> Confex.get_env(:deploy_notify_channels)
    |> Enum.each(&send_notification(client, &1, message))
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
