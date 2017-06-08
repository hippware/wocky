defmodule Wocky.Mixfile do
  use Mix.Project

  def project do
    [app: :wocky,
     version: version(),
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

  defp version do
    {ver_result, _} = System.cmd("elixir", ["../../version.exs"])
    ver_result
  end

  def application do
    [
      # Specify extra applications you'll use from Erlang/Elixir
      extra_applications: [:logger],
      mod: {Wocky.Application, []},
      env: [
        event_handler: Wocky.EventHandler,
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
      {:postgrex,             "~> 0.13.0"},
      {:poolboy,              "~> 1.5"},
      {:faker,                "~> 0.7.0"},
      {:ex_machina,           "~> 2.0"},
      {:ex_aws,               "~> 1.1"},
      {:pushex,               "~> 0.2.0"},
      {:configparser_ex,      "~> 1.0", override: true},
      {:sweet_xml,            "~> 0.6.5"},
      {:hackney,              "~> 1.7", override: true},
      {:exjsx,                "~> 3.2", override: true},
      {:algolia,              "~> 0.6"},
      {:geocalc,              "~> 0.5.3"},
      {:gen_stage,            "~> 0.11"},
      {:timex,                "~> 3.1"},
      {:stringprep,           "~> 1.0"},
      {:exconstructor,        "~> 1.0"},
      {:ok,                   "~> 1.2", runtime: false},
      {:lager,                "~> 3.2", override: true},
      {:logger_lager_backend, "~> 0.0.2"},
      {:email_checker,        "~> 0.1.0"},
      {:csv,                  "~> 1.4"},
      {:confex,               "~> 2.0"},
      {:quantum,              "~> 1.9"},
      {:comeonin,             "~> 3.0"},

      {:apns,
        github: "chvanikoff/apns4ex",
        branch: "master",
        override: true},
      {:ossp_uuid,
        github: "hippware/erlang-ossp-uuid",
        tag: "v1.0.1",
        manager: :rebar3},
      {:certifi,
        github: "hippware/erlang-certifi",
        branch: "working",
        manager: :rebar3,
        override: true},
      {:slackex,
        github: "hippware/slackex",
        branch: "master"},

      {:espec,       "~> 1.2", only: :test},
      {:excoveralls, "~> 0.6", only: :test},
      {:credo,       "~> 0.6", only: [:dev, :test], runtime: false},
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
