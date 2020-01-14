use Mix.Config

# Set a higher stacktrace during development. Avoid configuring such
# in production as building large stacktraces may be expensive.
config :phoenix, :stacktrace_depth, 20

# Exometer depends on `setup`, which wants to create data and log
# directories at startup. We can't prevent it from creating the directories,
# but we can put them somewhere out of the way.
config :setup,
  data_dir: '_build/#{Mix.env()}/data',
  log_dir: '_build/#{Mix.env()}/log'

config :honeybadger, environment_name: :dev
