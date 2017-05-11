defmodule Wocky.Mixfile do
  use Mix.Project

  def project do
    [app: :wocky,
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
      mod: {Wocky.Application, []},
      env: [
        event_handler: Wocky.EventHandler,
        tros_backend: Wocky.TROS.TestStore,
        tros_s3_bucket: "wocky-tros",
        tros_s3_access_key_id: nil,
        tros_s3_secret_key: nil,
        async_location_processing: false,
        enable_bot_event_notifications: false,
        enable_follow_me_updates: false,
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
      {:ecto,                 "~> 2.0"},
      {:ecto_homoiconic_enum, "~> 0.1.1"},
      {:mariaex,              "~> 0.8.1"},
      {:postgrex,             "~> 0.13.0"},
      {:poolboy,              "~> 1.5"},
      {:faker,                "~> 0.7.0"},
      {:ex_machina,           "~> 2.0"},
      {:ex_aws,               "~> 1.1"},
      {:sweet_xml,            "~> 0.6.5"},
      {:hackney,              "~> 1.7", override: true},
      {:exjsx,                "~> 3.2", override: true},
      {:algolia,              "~> 0.4.0"},
      {:honeybadger,          "~> 0.6"},
      {:geocalc,              "~> 0.5.3"},
      {:gen_stage,            "~> 0.11"},
      {:timex,                "~> 3.1"},
      {:stringprep,           "~> 1.0"},
      {:exconstructor,        "~> 1.0"},
      {:ok,                   "~> 1.2", runtime: false},
      {:lager,                "~> 3.2", override: true},
      {:logger_lager_backend, "~> 0.0.2"},
      {:email_checker,        "~> 0.1.0"},

      {:ossp_uuid,
        github: "hippware/erlang-ossp-uuid",
        tag: "v1.0.1",
        manager: :rebar3},
      {:certifi,
        github: "hippware/erlang-certifi",
        branch: "working",
        manager: :rebar3,
        override: true},
      {:p1_pgsql,
        github: "processone/p1_pgsql",
        tag: "1.1.2",
        manager: :rebar
      },

      {:espec,       "~> 1.2", only: :test},
      {:excoveralls, "~> 0.6", only: :test},
      {:credo,       "~> 0.6", only: :dev, runtime: false},
      {:ex_guard,    "~> 1.1", only: :dev, runtime: false},
      {:reprise,     "~> 0.5", only: :dev}
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
