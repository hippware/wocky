use Mix.Config

# Print only errors during test
config :logger,
  level: :error

config :lager,
  handlers: [
    lager_console_backend: [level: :critical]
  ]

config :honeybadger, environment_name: :test
