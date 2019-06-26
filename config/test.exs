use Mix.Config

# Print only errors during test
config :logger,
  compile_time_purge_level: :info,
  level: :error

config :lager,
  handlers: [
    lager_console_backend: [level: :critical]
  ]

config :honeybadger, environment_name: :test
