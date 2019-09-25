defmodule Wocky.Release.Mixfile do
  use Mix.Project

  def project do
    [
      apps_path: "apps",
      start_permanent: Mix.env() == :prod,
      test_coverage: [tool: ExCoveralls, test_task: "test"],
      preferred_cli_env: [
        coveralls: :test,
        "coveralls.html": :test,
        dialyzer: :test,
        check: :test
      ],
      dialyzer: [
        plt_add_apps: [:ex_unit],
        flags: [
          :error_handling,
          :race_conditions,
          :underspecs,
          :unmatched_returns
        ],
        ignore_warnings: "dialyzer_ignore.exs",
        list_unused_filters: true
      ],
      deps: deps(),
      aliases: aliases()
    ]
  end

  # Dependencies listed here are available only for this project
  # and cannot be accessed from applications inside the apps folder
  defp deps do
    [
      # TODO: Remove the following lines when the hackney SSL fix is
      # published to Hex
      {:hackney, github: "benoitc/hackney", branch: "master", override: true},
      {:idna, "6.0.0", override: true},
      {:mimerl, "1.2.0", override: true},
      {:certifi, "2.5.1", override: true},
      {:metrics, "2.5.0", override: true},
      {:ssl_verify_fun, "1.1.5", override: true},
      # END TODO
      {:credo, "~> 1.1", only: [:dev, :test], runtime: false},
      {:dialyxir, "~> 1.0.0-rc.4", only: [:dev, :test], runtime: false},
      {:ex_check, ">= 0.0.0", only: [:dev, :test], runtime: false},
      {:ex_guard, "~> 1.1", only: [:dev, :test], runtime: false},
      {:excoveralls, "~> 0.8", only: [:dev, :test], runtime: false},
      {:mix_test_watch, "~> 0.8", only: :dev, runtime: false}
    ]
  end

  defp aliases do
    [
      recompile: ["clean", "compile"],
      prepare: ["deps.get", "recompile"],
      "ecto.setup": ["ecto.create", "ecto.migrate"],
      "ecto.reset": ["ecto.drop", "ecto.setup"]
    ]
  end
end
