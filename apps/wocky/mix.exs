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
        espec: :test,
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
  defp elixirc_paths(:test), do: ["lib", "test/support", "spec/support"]
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
      {:algolia, "~> 0.6"},
      {:bamboo, "~> 0.8"},
      {:bcrypt_elixir, "~> 1.0"},
      {:comeonin, "~> 4.0"},
      {:confex, "~> 3.3"},
      {:csv, "~> 2.0"},
      {:dawdle, "~> 0.4.0"},
      {:ecto, "~> 2.2"},
      {:ecto_homoiconic_enum, "~> 0.1"},
      {:email_checker, "~> 0.1"},
      {:ex_aws, "~> 2.0"},
      {:ex_aws_s3, "~> 2.0"},
      {:ex_aws_sqs, "~> 2.0"},
      {:ex_json_logger, "~> 1.0"},
      {:ex_machina, "~> 2.1"},
      {:exconstructor, "~> 1.0"},
      {:faker, "~> 0.9"},
      {:guardian, "~> 1.0"},
      {:guardian_firebase, "~> 0.1.0"},
      {:gen_stage, "~> 0.12"},
      {:geo_postgis, "~> 2.0"},
      {:geocalc, "~> 0.5"},
      {:hackney, "~> 1.7"},
      {:honeybadger, "~> 0.6"},
      {:idna, "~> 5.1", override: true},
      {:joken, "~> 1.1"},
      {:jsx, "~> 2.8"},
      {:kadabra, "~> 0.3"},
      {:peerage, "~> 1.0"},
      {:pigeon, "~> 1.1"},
      {:poolboy, "~> 1.5"},
      {:postgrex, ">= 0.0.0"},
      {:prometheus_ecto, "~> 1.0"},
      {:prometheus_ex, "~> 1.4"},
      {:recon, "~> 2.3"},
      {:rexbug, "~> 1.0"},
      {:slack_ex, "~> 0.1"},
      {:stringprep, "~> 1.0"},
      {:sweet_xml, "~> 0.6"},
      {:timex, "~> 3.1"},
      {:xml_builder, "~> 2.0", override: true},
      {:cowlib, github: "hippware/cowlib", branch: "working", override: true},
      {:ossp_uuid,
       github: "hippware/erlang-ossp-uuid", tag: "v1.0.1", manager: :rebar3},
      {:uuid, "~> 1.7", hex: :uuid_erl, override: true},
      {:bypass, "~> 0.7", only: :test, runtime: false},
      {:credo, "~> 0.6", only: [:dev, :test], runtime: false},
      {:espec, "~> 1.5", only: :test},
      {:excoveralls, "~> 0.6", only: :test},
      {:exvcr, "~> 0.8", only: :test},
      {:ex_guard, "~> 1.1", only: :dev, runtime: false},
      {:meck, "~> 0.8", only: :test, override: true},
      {:reprise, "~> 0.5", only: :dev},
      {:distillery,
       github: "hippware/distillery",
       branch: "working",
       runtime: false,
       override: true},
      {:wocky_db_watcher,
       git: "https://github.com/hippware/wocky_db_watcher.git",
       branch: "master",
       app: false}
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
