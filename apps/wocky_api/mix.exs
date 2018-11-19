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
        espec: :test,
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

      # TODO: Move these back to the offical absinthe repo once all the changes
      # are merged.
      {:absinthe,
       github: "hippware/absinthe",
       branch: "subscription-catchup",
       override: true},
      {:absinthe_phoenix,
       github: "hippware/absinthe_phoenix", branch: "subscription-catchup"},
      {:absinthe_relay, "~> 1.5.0-alpha.0"},
      {:absinthe_ecto, "~> 0.1.3"},
      {:absinthe_metrics, "~> 0.9.0"},
      {:honeybadger, "~> 0.6"},
      {:httpoison, "~> 0.13"},
      {:phoenix, "~> 1.3"},
      {:phoenix_ecto, "~> 3.6"},
      {:phoenix_pubsub, "~> 1.0"},
      {:plug_cowboy, "~> 1.0"},
      {:prometheus_ex, "~> 3.0", override: true},
      {:phoenix_pubsub_redis,
       github: "phoenixframework/phoenix_pubsub_redis", branch: "master"},
      {:redix_pubsub, "~> 0.5", override: true},
      {:redix, "~> 0.8", override: true},

      # TODO: Move back to the official redlock repo once the changes are merged
      {:redlock, "~> 1.0.6"},
      {:prometheus_phoenix, "~> 1.2"},
      {:prometheus_plugs, "~> 1.1"},
      {:idna, "~> 6.0", override: true},
      {:ranch, "~> 1.7.0", override: true},
      {:cowlib, github: "hippware/cowlib", branch: "working", override: true},
      {:credo, "~> 1.0", only: [:dev, :test], runtime: false},
      {:espec, "~> 1.5", only: :test},
      {:excoveralls, "~> 0.6", only: :test},
      {:ex_guard, "~> 1.1", only: :dev, runtime: false},
      {:reprise, "~> 0.5", only: :dev},
      {:kronky, github: "mirego/kronky", branch: "master"},
      {:cors_plug, "~> 2.0"},
      {:apollo_tracing, "~> 0.4.0"},
      {:distillery, "~> 2.0-rc.8", runtime: false, override: true},
      {:bimap, "~> 1.0"},
      {:lager, "~> 3.6", override: true}
    ]
  end

  defp aliases do
    [
      recompile: ["clean", "compile"],
      "ecto.setup": ["ecto.create", "ecto.migrate", "run priv/repo/seeds.exs"],
      "ecto.reset": ["ecto.drop", "ecto.setup"],
      espec: ["ecto.create --quiet", "ecto.migrate", "espec"],
      test: ["ecto.create --quiet", "ecto.migrate", "test"]
    ]
  end
end
