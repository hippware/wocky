defmodule Wocky.Release.Mixfile do
  use Mix.Project

  def project do
    [
      apps_path: "apps",
      start_permanent: Mix.env == :prod,
      test_coverage: [tool: ExCoveralls, test_task: "espec"],
      preferred_cli_env: [
        espec: :test,
        ct: :test,
        coveralls: :test,
        "coveralls.html": :test
      ],
      dialyzer: [
        plt_add_apps: [:mix, :mnesia, :inets],
        plt_add_deps: :transitive,
        ignore_warnings: "dialyzer.ignore-warnings",
        flags: [
          # :unmatched_returns,
          # :underspecs,
          :error_handling,
          :race_conditions
        ]
      ],
      deps: deps(),
      aliases: aliases()
    ]
  end

  # Dependencies listed here are available only for this project
  # and cannot be accessed from applications inside the apps folder
  defp deps do
    [
      {:distillery,  "~> 1.1", runtime: false},
      {:dialyxir,    "~> 0.5", only: [:dev, :test], runtime: false},
      {:espec,       "~> 1.5", only: :test},
      {:excoveralls, "~> 0.6", only: :test},
      {:mix_ct,
        github: "hippware/mix_ct",
        branch: "master",
        only: :test}
    ]
  end

  defp aliases do
    [
      recompile: ["clean", "compile"],
      prepare: ["deps.get", "deps.compile goldrush lager", "recompile"],
      lint: ["elvis", "credo"],
      "ecto.setup": ["ecto.create", "ecto.migrate"],
      "ecto.reset": ["ecto.drop", "ecto.setup"],
      "ecto.wait": &wait_for_db/1,
      epmd: &run_epmd/1
    ]
  end

  defp wait_for_db(_) do
    do_wait_for_db(0)
  end

  defp do_wait_for_db(retries) do
    user = System.get_env("WOCKY_DB_USER") || "postgres"
    host = System.get_env("WOCKY_DB_HOST") || "localhost"

    "pg_isready"
    |> System.cmd(["-h", host, "-U", user])
    |> handle_db_result(retries)
  end

  defp handle_db_result({_, 0}, _), do: :ok
  defp handle_db_result({_, _}, 60), do: raise "PostgreSQL wasn't ready in time"
  defp handle_db_result({_, _}, retries) do
    Process.sleep(1000)
    do_wait_for_db(retries + 1)
  end

  defp run_epmd(_), do: System.cmd("epmd", ["-daemon"])
end
