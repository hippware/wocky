defmodule Wocky.Mixfile do
  use Mix.Project

  def project do
    [
      app: :wocky,
      version: version(),
      build_path: "../../_build",
      config_path: "../../config/config.exs",
      deps_path: "../../deps",
      lockfile: "../../mix.lock",
      elixir: "~> 1.7",
      elixirc_paths: elixirc_paths(Mix.env()),
      start_permanent: Mix.env() == :prod,
      test_coverage: [tool: ExCoveralls, test_task: "test"],
      preferred_cli_env: [
        coveralls: :test,
        "coveralls.html": :test,
        vcr: :test,
        "vcr.delete": :test,
        "vcr.check": :test,
        "vcr.show": :test
      ],
      aliases: aliases(),
      deps: deps()
    ]
  end

  defp version do
    {ver_result, _} = System.cmd("elixir", ["../../version.exs"])
    ver_result
  end

  # Specifies which paths to compile per environment.
  defp elixirc_paths(:test), do: ["lib", "test/support"]
  defp elixirc_paths(_), do: ["lib"]

  def application do
    [
      # Specify extra applications you'll use from Erlang/Elixir
      # Also include vaultex here to ensure it starts before any app that needs
      # to read secrets
      extra_applications: [:logger, :runtime_tools, :inets],
      included_applications: [],
      mod: {Wocky.Application, []},
      env: [
        wocky_env: {:system, "WOCKY_ENV", "dev"},
        wocky_inst: {:system, "WOCKY_INST", "local"},
        wocky_host: {:system, "WOCKY_HOST", "localhost"},
        reserved_handles: [
          "root",
          "admin",
          "super",
          "superuser",
          "tinyrobot",
          "hippware",
          "www",
          "support",
          "null"
        ]
      ]
    ]
  end

  defp deps do
    [
      {:bamboo, "~> 1.0"},
      {:benchee, "~> 1.0", only: :dev},
      {:bcrypt_elixir, "~> 2.0"},
      {:bimap, "~> 1.0"},
      {:confex, github: "Nebo15/confex", branch: "master", override: true},
      {:configparser_ex, "~> 4.0", only: [:dev, :test]},
      {:dataloader, "~> 1.0.0"},
      {:dawdle, "~> 0.6"},
      {:dawdle_db, "~> 0.6"},
      {:distillery, "~> 2.0"},
      {:ecto_enum, "~> 1.3"},
      {:ecto_homoiconic_enum,
       github: "hippware/ecto_homoiconic_enum", branch: "master"},
      {:ecto_sql, "~> 3.0"},
      {:elixometer, github: "hippware/elixometer", branch: "working"},
      {:email_checker, "~> 0.1"},
      {:eventually, "~> 1.0"},
      {:ex_aws, "~> 2.0"},
      {:ex_aws_s3, "~> 2.0"},
      {:ex_aws_sqs, "~> 2.0"},
      {:ex_json_logger, "~> 1.0"},
      {:ex_machina, "~> 2.1"},
      {:ex_phone_number, "~> 0.1"},
      {:ex_twilio, "~> 0.7"},
      {:exconstructor, "~> 1.0"},
      {:exometer_core,
       github: "hippware/exometer_core", branch: "working", override: true},
      {:exometer_prometheus,
       github: "GalaxyGorilla/exometer_prometheus",
       branch: "master",
       manager: :rebar3},
      {:exprof, "~> 0.2", only: :dev},
      {:exrun, "~> 0.1.6"},
      {:faker, "~> 0.9"},
      {:firebase_admin_ex,
       github: "scripbox/firebase-admin-ex", branch: "master"},
      {:fun_with_flags, github: "hippware/fun_with_flags", branch: "confex"},
      {:gen_stage, "~> 0.12"},
      {:geo_postgis, "~> 3.0"},
      {:geocalc, "~> 0.5"},
      {:guardian, "~> 1.0"},
      {:guardian_firebase, "~> 0.2.1"},
      {:honeybadger, "~> 0.6"},
      {:kadabra, "~> 0.3"},
      {:module_config, "~> 0.1.0", github: "hippware/module_config"},
      {:observer_cli, "~> 1.5"},
      # TODO: Move back to release when new release is built
      {:paginator, github: "duffelhq/paginator", branch: "master"},
      {:peerage, "~> 1.0"},
      {:phoenix_pubsub, "~> 1.0"},
      {:phoenix_pubsub_redis, "~> 2.1.5"},
      {:pigeon, "~> 1.3"},
      {:plug, "~> 1.0"},
      {:plug_cowboy, "~> 2.0"},
      {:postgrex, ">= 0.0.0"},
      {:prometheus_ecto, "~> 1.4"},
      {:prometheus_ex, "~> 3.0"},
      {:prometheus_process_collector, "~> 1.4"},
      {:recon, "~> 2.3"},
      {:redix, "~> 0.9.2"},
      {:redlock, "~> 1.0.9"},
      {:rexbug, ">= 1.0.0"},
      # TODO: go back to hex version once changes are merged
      {:slack_ex, github: "hippware/slack_ex", branch: "master"},
      {:stringprep, "~> 1.0"},
      {:swarm, "~> 3.0"},
      {:sweet_xml, "~> 0.6"},
      {:timex, "~> 3.1"},
      {:vaultex, "~> 0.12"},
      # Non-prod
      {:bypass, "~> 1.0", only: :test, runtime: false},
      {:credo, "~> 1.0", only: [:dev, :test], runtime: false},
      {:credo_naming, "~> 0.3", only: [:dev, :test], runtime: false},
      {:ex_unit_notifier, "~> 0.1", only: :test, runtime: false},
      {:excoveralls, "~> 0.6", only: :test},
      {:meck, "~> 0.8", only: :test},
      {:mock, "~> 0.3", only: :test},
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
