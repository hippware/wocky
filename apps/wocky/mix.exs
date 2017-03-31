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
     test_coverage: [tool: Coverex.Task],
     preferred_cli_env: [
       espec: :test
     ],
     dialyzer: [
       plt_apps: [:ecto],
       plt_add_deps: :transitive,
       flags: [
         # :unmatched_returns,
         # :underspecs,
         :error_handling,
         :race_conditions
       ]
     ],
     aliases: aliases(),
     deps: deps()]
  end

  def application do
    [
      # Specify extra applications you'll use from Erlang/Elixir
      extra_applications: [:logger],
      mod: {Wocky.Application, []},
      env: [
        location_api_port: 8080,
        enable_follow_me: false,
        algolia_user_index_name: [
          staging: 'dev_wocky_users',
          us1:     'prod_wocky_users'
        ],
        algolia_bot_index_name: [
          staging: 'dev_wocky_bots',
          us1:     'prod_wocky_bots'
        ],
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
      {:ecto,           "~> 2.0"},
      {:mariaex,        "~> 0.8.1"},
      {:poolboy,        "~> 1.5"},
      {:faker,          "~> 0.7.0"},
      {:ex_machina,     "~> 2.0"},
      {:hackney,        "~> 1.6", override: true},
      {:algolia,        "~> 0.4.0"},
      {:geocalc,        "~> 0.5.3"},
      {:honeybadger,    "~> 0.6"},
      {:sweet_xml,      "~> 0.6.5"},
      {:exconstructor,  "~> 1.0"},
      {:ok,             "~> 1.2", runtime: false},
      {:exactor,        "~> 2.2", runtime: false},

      {:ossp_uuid,
        github: "hippware/erlang-ossp-uuid",
        tag: "v1.0.1",
        manager: :rebar3},
      {:ex_aws,
        github: "CargoSense/ex_aws",
        branch: "master",
        override: true},

      {:espec,      "~> 1.2", only: :test},
      {:coverex,    "~> 1.4", only: :test},
      {:credo,      "~> 0.6", only: :dev, runtime: false},
      {:ex_guard,   "~> 1.1", only: :dev, runtime: false},
      {:dialyxir,   "~> 0.4", only: :dev, runtime: false},
      {:reprise,    "~> 0.5", only: :dev}
    ]
  end

  defp aliases do
    [
      "recompile": ["clean", "compile"],
      "ecto.setup": ["ecto.create", "ecto.migrate", "run priv/repo/seeds.exs"],
      "ecto.reset": ["ecto.drop", "ecto.setup"],
      "espec": ["ecto.create --quiet", "ecto.migrate", "espec"]
    ]
  end
end
