use Mix.Config

# Print only errors during test
config :logger,
  compile_time_purge_level: :info,
  level: :error

config :lager,
  handlers: [
    lager_console_backend: [level: :critical]
  ]

# Exometer depends on `setup`, which wants to create data and log
# directories at startup. We can't prevent it from creating the directories,
# but we can put them somewhere out of the way.
config :setup,
  data_dir: '_build/#{Mix.env()}/data',
  log_dir: '_build/#{Mix.env()}/log'

config :honeybadger, environment_name: :test
