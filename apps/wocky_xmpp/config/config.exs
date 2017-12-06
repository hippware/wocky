# This file is responsible for configuring your application
# and its dependencies with the aid of the Mix.Config module.
use Mix.Config

config :wocky_xmpp,
  ecto_repos: []

config :wocky_xmpp, :redis,
  host:      {:system, :string,  "REDIS_HOST",      "localhost"},
  port:      {:system, :integer, "REDIS_PORT",      6379},
  db:        {:system, :integer, "REDIS_DB",        0},
  pool_size: {:system, :integer, "REDIS_POOL_SIZE", 10}

config :ejabberd,
  keep_lager_intact: true

config :kernel,
  start_pg2: :true

config :ssl,
  session_lifetime: 600 # 10 minutes

config :mnesia,
  dir: 'log/#{Mix.env}/mnesia'

config :setup,
  verify_directories: false

config :alarms,
  large_heap: 10_000_000

config :honeybadger,
  use_logger: true

config :exometer,
  mongooseim_report_interval: 300_000, # 5 minutes
  report: [
    reporters: [
      exometer_report_prometheus: [:enable_httpd]
    ]
  ]

import_config "#{Mix.env}.exs"
