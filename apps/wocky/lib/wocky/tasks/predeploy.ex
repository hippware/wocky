defmodule Wocky.Tasks.Predeploy do
  @moduledoc "Tasks to run before a new deployment is launched"

  alias Ecto.Migrator
  alias Slack.Chat
  alias Wocky.Repo

  def run do
    :ok = Application.load(:wocky)

    notify()
    migrate()

    :init.stop()
  end

  defp notify do
    Application.ensure_all_started(:slack_ex)
    client = :wocky |> Confex.get_env(:slack_token) |> Slack.client()

    version = Application.spec(:wocky, :vsn)
    wocky_env = Confex.get_env(:wocky, :wocky_env)
    wocky_inst = Confex.get_env(:wocky, :wocky_inst)
    instance_name = "#{wocky_inst}.#{wocky_env}"
    message = "Deploying Wocky release `#{version}` to `#{instance_name}`"

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

  defp migrate do
    Enum.each([:postgrex, :ecto], &Application.ensure_all_started/1)
    Repo.start_link(pool_size: 1)

    Migrator.run(Repo, migrations_path(:wocky), :up, all: true)

    seed_script = seeds_path(:wocky)

    if File.exists?(seed_script) do
      Code.eval_file(seed_script)
    end
  end

  defp priv_dir(app), do: "#{:code.priv_dir(app)}"

  defp migrations_path(app),
    do: Path.join([priv_dir(app), "repo", "migrations"])

  defp seeds_path(app), do: Path.join([priv_dir(app), "repo", "seeds.exs"])
end
