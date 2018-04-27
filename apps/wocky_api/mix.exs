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
      {:absinthe,
        github: "hippware/absinthe",
        branch: "master",
        override: true},
      {:absinthe_phoenix, "~> 1.4"},
      {:absinthe_relay, github: "hippware/absinthe_relay", branch: "master"},
      {:absinthe_ecto, "~> 0.1.3"},
      {:cowboy, "~> 1.0"},
      {:honeybadger, "~> 0.6"},
      {:httpoison, "~> 0.13"},
      {:phoenix, "~> 1.3"},
      {:phoenix_ecto, "~> 3.2"},
      {:phoenix_pubsub, "~> 1.0"},
      {:prometheus_phoenix, "~> 1.2"},
      {:prometheus_plugs, "~> 1.1"},
      {:idna, "~> 5.1", override: true},
      {:ranch, "~> 1.3.2", override: true},
      {:cowlib, github: "hippware/cowlib", branch: "working", override: true},
      {:credo, "~> 0.6", only: [:dev, :test], runtime: false},
      {:espec, "~> 1.5", only: :test},
      {:excoveralls, "~> 0.6", only: :test},
      {:ex_guard, "~> 1.1", only: :dev, runtime: false},
      {:reprise, "~> 0.5", only: :dev},
      {:kronky, "~> 0.5.0"},
      {:cors_plug, "~> 1.5"}
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
