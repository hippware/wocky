defmodule WockyAPI.Mixfile do
  use Mix.Project

  def project do
    [
      app: :wocky_api,
      version: version(),
      build_path: "../../_build",
      config_path: "../../config/config.exs",
      deps_path: "../../deps",
      lockfile: "../../mix.lock",
      elixir: "~> 1.4",
      elixirc_paths: elixirc_paths(Mix.env()),
      compilers: [:phoenix] ++ Mix.compilers(),
      start_permanent: Mix.env() == :prod,
      test_coverage: [tool: ExCoveralls, test_task: "test"],
      preferred_cli_env: [
        coveralls: :test,
        "coveralls.html": :test
      ],
      elvis_config: [%{src_dirs: [], rules: []}],
      aliases: aliases(),
      deps: deps()
    ]
  end

  def application do
    [
      # Specify extra applications you'll use from Erlang/Elixir
      extra_applications: [:logger, :runtime_tools],
      mod: {WockyAPI.Application, []},
      env: []
    ]
  end

  defp version do
    {ver_result, _} = System.cmd("elixir", ["../../version.exs"])
    ver_result
  end

  # Specifies which paths to compile per environment.
  defp elixirc_paths(:test), do: ["lib", "test/support"]
  defp elixirc_paths(_), do: ["lib"]

  defp deps do
    [
      {:wocky, in_umbrella: true},
      {:bimap, "~> 1.0"},
      # TODO: Move these back to the offical absinthe repo once all the changes
      # are merged.
      {:absinthe,
       github: "hippware/absinthe",
       branch: "subscription-catchup",
       override: true},
      {:absinthe_phoenix,
       github: "hippware/absinthe_phoenix", branch: "subscription-catchup"},
      {:absinthe_relay,
       github: "absinthe-graphql/absinthe_relay", branch: "master"},
      {:absinthe_ecto, "~> 0.1.3"},
      {:absinthe_metrics, "~> 0.9.0"},
      {:apollo_tracing, "~> 0.4.0"},
      {:cors_plug, "~> 2.0"},
      {:ecto_sql, "~> 3.0"},
      {:honeybadger, "~> 0.6"},
      {:jason, "~> 1.0"},
      {:kronky, github: "mirego/kronky", branch: "master"},
      {:phoenix, "~> 1.4"},
      {:phoenix_ecto, "~> 4.0"},
      {:phoenix_pubsub, "~> 1.0"},
      # TODO: Back to the phoenixframework/ version once the update is merged
      {:phoenix_pubsub_redis,
       github: "hippware/phoenix_pubsub_redis", branch: "redix-0.9"},
      {:plug_cowboy, "~> 2.0"},
      {:plug, "~> 1.7"},
      {:prometheus_ex, "~> 3.0", override: true},
      {:prometheus_phoenix, "~> 1.2"},
      {:prometheus_plugs, "~> 1.1"},
      {:redix, "~> 0.9", override: true},
      {:redlock, "~> 1.0.6"},
      {:credo, "~> 1.0", only: [:dev, :test], runtime: false},
      {:excoveralls, "~> 0.6", only: :test},
      {:ex_guard, "~> 1.1", only: :dev, runtime: false},
      {:reprise, "~> 0.5", only: :dev}
    ]
  end

  defp aliases do
    [
      recompile: ["clean", "compile"],
      "ecto.setup": ["ecto.create", "ecto.migrate", "run priv/repo/seeds.exs"],
      "ecto.reset": ["ecto.drop", "ecto.setup"],
      test: ["ecto.create --quiet", "ecto.migrate", "test"]
    ]
  end
end
