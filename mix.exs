defmodule Wocky.Release.Mixfile do
  use Mix.Project

  def project do
    [
      apps_path: "apps",
      build_embedded: false, # Mix.env == :prod,
      start_permanent: Mix.env == :prod,
      preferred_cli_env: [
        espec: :test,
        ct: :test,
        coveralls: :test,
        "coveralls.html": :test,
        release: :prod
      ],
      deps: deps(),
      aliases: aliases(),
      test_coverage: [tool: ExCoveralls, test_task: "espec"],
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
    ]
  end

  # Dependencies listed here are available only for this project
  # and cannot be accessed from applications inside the apps folder
  defp deps do
    [
      {:distillery, "~> 1.1", runtime: false},
      {:dialyxir,   "~> 0.5", only: :dev, runtime: false},
      {:espec,       "~> 1.2", only: :test},
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
      prepare: ["deps.get", "deps.compile goldrush lager", "compile"],
      lint: ["elvis", "credo"]
    ]
  end
end
