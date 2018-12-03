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
      elixir: "~> 1.4",
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
      elvis_config: [%{src_dirs: [], rules: []}],
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
      extra_applications: [:logger, :runtime_tools, :inets],
      included_applications: [
        # This one we use files from but don't actually want the application
        # running
        :wocky_db_watcher
      ],
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
        ],
        hs_prepopulation_user: "__new_user_hs_archive__",
        hs_prepopulation_days: 28,
        hs_prepopulation_min_items: 10
      ]
    ]
  end

  defp deps do
    [
      {:bamboo, "~> 1.0"},
      {:bcrypt_elixir, "~> 1.0"},
      {:comeonin, "~> 4.0"},
      {:confex, github: "Nebo15/confex", branch: "master", override: true},
      {:csv, "~> 2.0"},
      {:dawdle, "~> 0.4.0"},
      {:ecto_homoiconic_enum,
       github: "hippware/ecto_homoiconic_enum", branch: "master"},
      {:ecto_sql, "~> 3.0"},
      {:elixometer, github: "hippware/elixometer", branch: "working"},
      {:email_checker, "~> 0.1"},
      {:ex_aws, "~> 2.0"},
      {:ex_aws_s3, "~> 2.0"},
      {:ex_aws_sqs, "~> 2.0"},
      {:ex_json_logger, "~> 1.0"},
      {:ex_machina, "~> 2.1"},
      {:exconstructor, "~> 1.0"},
      # TODO: This dependency is only used in one migration. We should remove
      # it after checkpointing the database schema.
      {:exml, github: "esl/exml", tag: "3.0.3", manager: :rebar3},
      {:exometer_core,
       github: "hippware/exometer_core", branch: "working", override: true},
      {:exometer_prometheus,
       github: "GalaxyGorilla/exometer_prometheus",
       branch: "master",
       manager: :rebar3},
      {:exrun, "~> 0.1.6"},
      {:faker, "~> 0.9"},
      # TODO: go back to hex version once changes are merged
      {:firebase_admin_ex,
        github: "hippware/firebase-admin-ex",
        branch: "master"},
      {:gen_stage, "~> 0.12"},
      {:geo_postgis, "~> 3.0"},
      {:geocalc, "~> 0.5"},
      {:guardian, "~> 1.0"},
      {:guardian_firebase, "~> 0.2.1"},
      {:honeybadger, "~> 0.6"},
      {:joken, "~> 1.1"},
      {:kadabra, "~> 0.3"},
      {:lager_logger, "~> 1.0"},
      {:peerage, "~> 1.0"},
      {:pigeon, "~> 1.1"},
      {:plug, "~> 1.0"},
      {:plug_cowboy, "~> 2.0"},
      {:postgrex, ">= 0.0.0"},
      # TODO: Re-enable when prometheus_ecto supports Ecto 3
      # {:prometheus_ecto, "~> 1.0"},
      {:prometheus_ex, "~> 3.0"},
      {:recon, "~> 2.3"},
      {:redix, "~> 0.9", override: true},
      # TODO: go back to hex version once changes are merged
      {:slack_ex, github: "hippware/slack_ex", branch: "master"},
      {:stringprep, "~> 1.0"},
      {:sweet_xml, "~> 0.6"},
      {:timex, "~> 3.1"},
      {:wocky_db_watcher,
       git: "https://github.com/hippware/wocky_db_watcher.git",
       branch: "master",
       app: false},

      {:bypass, "~> 1.0", only: :test, runtime: false},
      {:credo, "~> 1.0", only: [:dev, :test], runtime: false},
      {:excoveralls, "~> 0.6", only: :test},
      {:ex_guard, "~> 1.1", only: :dev, runtime: false},
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
