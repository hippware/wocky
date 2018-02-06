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
      test_coverage: [tool: ExCoveralls, test_task: "espec"],
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
      extra_applications: [:lager, :logger, :runtime_tools],
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
      {:postgrex, ">= 0.0.0"},
      {:ecto, "~> 2.2"},
      {:ecto_homoiconic_enum, "~> 0.1"},
      {:poolboy, "~> 1.5"},
      {:faker, "~> 0.9"},
      {:ex_machina, "~> 2.1"},
      {:ex_aws, "~> 2.0"},
      {:ex_aws_s3, "~> 2.0"},
      {:ex_aws_sqs, "~> 2.0"},
      {:pigeon, "~> 1.1"},
      {:kadabra, "~> 0.3"},
      {:configparser_ex, "~> 2.0", override: true},
      {:sweet_xml, "~> 0.6"},
      {:hackney, "~> 1.7", override: true},
      {:exjsx, "~> 4.0", override: true},
      {:algolia, "~> 0.6"},
      {:geo_postgis, "~> 1.0"},
      {:geocalc, "~> 0.5"},
      {:gen_stage, "~> 0.12.0"},
      {:timex, "~> 3.1"},
      {:stringprep, "~> 1.0"},
      {:exconstructor, "~> 1.0"},
      {:ok, "~> 1.2", runtime: false},
      {:lager, "~> 3.2", override: true},
      {:lager_logger, "~> 1.0"},
      {:ex_json_logger, "~> 1.0"},
      {:email_checker, "~> 0.1"},
      {:csv, "~> 2.0"},
      {:confex, "~> 3.3"},
      {:comeonin, "~> 4.0"},
      {:bcrypt_elixir, "~> 1.0"},
      {:joken, "~> 1.1"},
      {:peerage, "~> 1.0"},
      {:bamboo, "~> 0.8"},
      {:slack_ex, "~> 0.1"},
      {:prometheus_ex, "~> 1.4"},
      {:prometheus_ecto, "~> 1.0"},
      {:geo, github: "bryanjos/geo", branch: "master", override: true},
      {:ossp_uuid,
       github: "hippware/erlang-ossp-uuid", tag: "v1.0.1", manager: :rebar3},
      {:certifi,
       github: "hippware/erlang-certifi",
       branch: "working",
       manager: :rebar3,
       override: true},
      {:cowlib, github: "hippware/cowlib", branch: "working", override: true},
      {:espec, "~> 1.5", only: :test},
      {:excoveralls, "~> 0.6", only: :test},
      {:bypass, "~> 0.7", only: :test, runtime: false},
      {:exvcr, "~> 0.8", only: :test},
      {:credo, "~> 0.6", only: [:dev, :test], runtime: false},
      {:ex_guard, "~> 1.1", only: :dev, runtime: false},
      {:reprise, "~> 0.5", only: :dev},
      {:meck, "~> 0.8.8", only: :test, override: true},
      {:wocky_db_watcher,
       git: "git@github.com:hippware/wocky_db_watcher",
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
