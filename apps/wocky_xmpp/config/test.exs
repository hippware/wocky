use Mix.Config

config :wocky_xmpp,
  wocky_env: 'test'

config :honeybadger,
  environment_name: :test

config :logger,
  level: :error

config :lager,
  handlers: [
    lager_console_backend: [
      level: :critical,
      formatter: :lager_default_formatter,
      formatter_config: [:time, :color, ' [', :severity, '] ', :message, '\e[0m\r\n']
    ],
    lager_file_backend: [
      file: 'wocky.log',
      level: :info
    ],
    lager_file_backend: [
      file: 'debug.log',
      level: :debug
    ]
  ]
