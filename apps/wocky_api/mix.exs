defmodule WockyAPI.Mixfile do
  use Mix.Project

  def project do
    [app: :wocky_api,
     version: "0.1.0",
     build_path: "../../_build",
     config_path: "../../config/config.exs",
     deps_path: "../../deps",
     lockfile: "../../mix.lock",
     elixir: "~> 1.4",
     build_embedded: Mix.env == :prod,
     start_permanent: Mix.env == :prod,
     test_coverage: [tool: ExCoveralls, test_task: "espec"],
     preferred_cli_env: [
       espec: :test,
       coveralls: :test,
       "coveralls.html": :test
     ],
     elvis_config: [%{src_dirs: [], rules: []}],
     aliases: aliases(),
     deps: deps()]
  end

  def application do
    [
      # Specify extra applications you'll use from Erlang/Elixir
      extra_applications: [:logger],
      mod: {WockyAPI.Application, []},
      env: [
        location_api_port: 8080
      ]
    ]
  end

  defp deps do
    [
      {:wocky,      in_umbrella: true},
      {:cowboy,     "~> 1.0"},
      {:poison,     "~> 2.2"},
      {:hackney,    "~> 1.7", override: true},
      {:ok,         "~> 1.2", runtime: false},
      {:lager,      "~> 3.2", override: true},
      {:logger_lager_backend, "~> 0.0.2"},

      {:espec,      "~> 1.2", only: :test},
      {:excoveralls,"~> 0.6", only: :test},
      {:credo,      "~> 0.6", only: :dev, runtime: false},
      {:ex_guard,   "~> 1.1", only: :dev, runtime: false},
      {:reprise,    "~> 0.5", only: :dev}
    ]
  end

  defp aliases do
    [
      "recompile": ["clean", "compile"],
      "ecto.setup": ["ecto.create", "ecto.migrate", "run priv/repo/seeds.exs"],
      "ecto.reset": ["ecto.drop", "ecto.setup"],
      "espec": ["ecto.create --quiet", "ecto.migrate", "espec"],
      "test": ["espec"]
    ]
  end
end
