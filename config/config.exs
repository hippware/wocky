# This file is responsible for configuring your application
# and its dependencies with the aid of the Mix.Config module.
use Mix.Config

config :wocky,
  start_ejabberd: :true,
  keyspace_prefix: 'wocky_'

config :cqerl,
  maps: :true,
  mode: :hash,
  text_uuids: true,
  auth: {:cqerl_auth_plain_handler, [{'cassandra', 'cassandra'}]}

config :schemata,
  cassandra_hosts: [{'127.0.0.1', 9042}],
  cassandra_opts: []

config :kernel,
  start_pg2: :true

config :sasl,
  sasl_error_logger: :false

config :mnesia,
  dir: 'data/mnesia'

config :setup,
  verify_directories: :false

config :lager,
  colored: :true,
  log_root: 'log',
  crash_log: 'crash.log',
  handlers: [
    {:lager_console_backend, [
       :info,
       {:lager_default_formatter, [:time, :color, ' [', :severity, '] ', :message, '\e[0m\r\n']}
    ]},
    {:lager_file_backend, [
      {:file, 'wocky.log'},
      {:level, :warning},
      {:size, 2097152},
      {:date, '$D0'},
      {:count, 5}
    ]},
    {:lager_file_backend, [
      {:file, 'debug.log'},
      {:level, :debug},
      {:size, 2097152},
      {:date, '$D0'},
      {:count, 5}
    ]}
  ]

config :hut,
  level: :critical

import_config "#{Mix.env}.exs"
