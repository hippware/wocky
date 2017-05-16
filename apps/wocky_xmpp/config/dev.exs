use Mix.Config

config :wocky_xmpp,
  wocky_env: 'dev'

config :lager,
  extra_sinks: [
    error_logger_lager_event: [
      handlers: [
        lager_file_backend: [file: 'error_logger.log', level: :info]
      ]
    ]
  ]
