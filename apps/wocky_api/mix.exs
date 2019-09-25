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

      # TODO: Remove the following lines when the hackney SSL fix is
      # published to Hex
      {:hackney, github: "benoitc/hackney", branch: "master", override: true},
      {:idna, "6.0.0", override: true},
      {:mimerl, "1.2.0", override: true},
      {:certifi, "2.5.1", override: true},
      {:metrics, "2.5.0", override: true},
      {:ssl_verify_fun, "1.1.5", override: true},
      # END TODO
      # TODO: Move these back to the offical absinthe repo once all the changes
      # are merged.
      {:absinthe,
       github: "hippware/absinthe", branch: "working", override: true},
      {:absinthe_error_payload, "~> 1.0"},
      {:absinthe_metrics, "~> 1.0.0"},
      {:absinthe_phoenix,
       github: "hippware/absinthe_phoenix", branch: "working"},
      {:absinthe_plug, "~> 1.5.0-alpha.0", override: true},
      {:absinthe_relay, github: "hippware/absinthe_relay", branch: "working"},
      {:apollo_tracing, "~> 0.4.0"},
      {:cors_plug, "~> 2.0"},
      {:ecto_sql, "~> 3.0"},
      {:health_checkup, "~> 0.1.0"},
      {:honeybadger, "~> 0.6"},
      {:httpoison, "~> 1.4"},
      {:jason, "~> 1.0"},
      {:phoenix, "~> 1.4"},
      {:phoenix_ecto, "~> 4.0"},
      {:plug, "~> 1.7"},
      {:plug_cowboy, "~> 2.0"},
      {:prometheus_ex, "~> 3.0", override: true},
      {:prometheus_phoenix, "~> 1.2"},
      {:prometheus_plugs, "~> 1.1"},
      # Non-prod
      {:credo, "~> 1.0", only: [:dev, :test], runtime: false},
      {:ex_guard, "~> 1.1", only: :dev, runtime: false},
      {:excoveralls, "~> 0.6", only: :test},
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
