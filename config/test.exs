use Mix.Config

# Print only errors during test
config :logger,
  compile_time_purge_level: :error,
  level: :error
