# This file is responsible for configuring your application
# and its dependencies with the aid of the Mix.Config module.
use Mix.Config

config :wocky_xmpp,
  ecto_repos: [],
  catchup_limit: {:system, :integer, "WOCKY_CATCHUP_LIMIT", 100}

config :wocky_xmpp, :redis,
  host: {:system, :string, "REDIS_HOST", "localhost"},
  port: {:system, :integer, "REDIS_PORT", 6379},
  db: {:system, :integer, "REDIS_DB", 0},
  pool_size: {:system, :integer, "REDIS_POOL_SIZE", 10}

config :mongooseim, keep_lager_intact: true

config :kernel, start_pg2: true

config :ssl,
  # 10 minutes
  session_lifetime: 600

config :mnesia, dir: 'log/#{Mix.env()}/mnesia'

config :honeybadger, use_logger: true

config :exometer,
  # 5 minutes
  mongooseim_report_interval: 300_000,
  report: [
    reporters: [
      exometer_report_prometheus: [:enable_httpd]
    ]
  ]

import_config "#{Mix.env()}.exs"
