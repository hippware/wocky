defmodule Wocky.Release.Mixfile do
  use Mix.Project

  def project do
    [
      apps_path: "apps",
      start_permanent: Mix.env() == :prod,
      test_coverage: [tool: ExCoveralls, test_task: "test"],
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
      {:distillery, "~> 2.0-rc.8", runtime: false, override: true},
      {:dialyxir, "~> 0.5", only: [:dev, :test], runtime: false},
      {:espec, "~> 1.5", only: :test},
      {:excoveralls, "~> 0.8", only: :test},
      {:mix_ct, github: "hippware/mix_ct", branch: "master", only: :test},

      # The apps below are required by multiple child apps (usually transitively
      # in at least one). We pin the version here to make sure they agree on
      # one.
      {:exometer_core,
       github: "hippware/exometer_core", branch: "working", override: true},
      {:lager, "~> 3.6", override: true}
    ]
  end

  defp aliases do
    [
      recompile: ["clean", "compile"],
      prepare: ["deps.get", "recompile"],
      lint: ["elvis", "credo --strict"],
      "ecto.setup": ["ecto.create", "ecto.migrate"],
      "ecto.reset": ["ecto.drop", "ecto.setup"],
      epmd: &run_epmd/1
    ]
  end

  defp run_epmd(_), do: System.cmd("epmd", ["-daemon"])
end
