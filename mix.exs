defmodule Wocky.Release.Mixfile do
  use Mix.Project

  def project do
    [
      apps_path: "apps",
      build_embedded: Mix.env == :prod,
      start_permanent: Mix.env == :prod,
      preferred_cli_env: [release: :prod],
      deps: deps(),
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
      ]
    ]
  end

  # Dependencies listed here are available only for this project
  # and cannot be accessed from applications inside the apps folder
  defp deps do
    [
      {:distillery, "~> 1.1"},
      {:dialyxir,   "~> 0.5"}
    ]
  end
end
