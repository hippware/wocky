# This file is responsible for configuring your application
# and its dependencies with the aid of the Mix.Config module.
use Mix.Config

config :distillery,
  no_warn_missing: [
    :distillery,
    :exactor,
    :dialyxir,
    :edown,
    :escalus,
    :exref,
    :fun_chain,
    :ok,
    :hamcrest,
    :meck,
    :mix_ct,
    :mix_elvis,
    :mix_eunit,
    :mustache,
    :pa,
    :parse_trans,
    :proper,
    :protobuffs,
    :riak_pb,
    :riakc
  ]

config :wocky,
  keyspace_prefix: 'wocky_'

config :algolia,
  application_id: "HIE75ZR7Q7",
  api_key: "ALGOLIA_KEY"

config :schemata,
  drop_nulls: false

config :ejabberd,
  keep_lager_intact: true

config :kernel,
  start_pg2: :true

config :sasl,
  sasl_error_logger: :false

config :mnesia,
  dir: '_build/#{Mix.env}/mnesia'

config :setup,
  verify_directories: :false

config :porcelain,
  driver: Porcelain.Driver.Basic,
  goon_warn_if_missing: false

config :logger,
  handle_otp_reports: false,
  backends: [LoggerLagerBackend],
  level: :debug

config :lager,
  colored: :true,
  log_root: '_build/#{Mix.env}/log',
  crash_log: 'crash.log',
  handlers: [
    lager_console_backend: [
       :info,
       {:lager_default_formatter, [:time, :color, ' [', :severity, '] ', :message, '\e[0m\r\n']}
    ],
    lager_file_backend: [
      file: 'wocky.log',
      level: :warning,
      size: 2097152,
      date: '$D0',
      count: 5
    ],
    lager_file_backend: [
      file: 'debug.log',
      level: :debug,
      size: 2097152,
      date: '$D0',
      count: 5
    ]
  ]

config :hut,
  level: :critical

config :honeybadger,
  exclude_envs: [:dev, :test],
  api_key: "HONEYBADGER_KEY"

import_config "#{Mix.env}.exs"
