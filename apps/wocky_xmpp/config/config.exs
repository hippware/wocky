# This file is responsible for configuring your application
# and its dependencies with the aid of the Mix.Config module.
use Mix.Config

config :wocky_xmpp,
  ecto_repos: [],
  hs_prepopulation_user: "__new_user_hs_archive__"

config :ejabberd,
  keep_lager_intact: true

config :kernel,
  start_pg2: :true

config :sasl,
  sasl_error_logger: false

config :ssl,
  session_lifetime: 600 # 10 minutes

config :mnesia,
  dir: 'log/#{Mix.env}/mnesia'

config :setup,
  verify_directories: false

config :hut,
  level: :critical

config :logger,
  handle_otp_reports: true,
  backends: [LoggerLagerBackend],
  level: :debug

config :lager,
  colored: true,
  log_root: 'log/#{Mix.env}',
  crash_log: 'crash.log',
  # Stop lager redirecting :error_logger messages
  error_logger_redirect: false,
  # Stop lager removing Logger's :error_logger handler
  error_logger_whitelist: [Logger.ErrorHandler],
  suppress_application_start_stop: true,
  suppress_supervisor_start_stop: true,
  handlers: [
    lager_console_backend: [
      level: :info,
      formatter: :lager_default_formatter,
      formatter_config: [:time, :color, ' [', :severity, '] ', :message, '\e[0m\r\n']
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
  ],
  traces: [
    {{:lager_file_backend, 'location.log'}, [module: Wocky.User.Location]},
    {{:lager_file_backend, 'location.log'}, [module: WockyAPI.LocationAPI]},
    {{:lager_file_backend, 'notifications.log'}, [
      module: Wocky.PushNotifier
    ]},
    {{:lager_file_backend, 'notifications.log'}, [
      module: Wocky.PushEventHandler
    ]},
    {{:lager_file_backend, 'notifications.log'}, [
      module: :mod_wocky_notifications
    ]}
  ]

import_config "#{Mix.env}.exs"
