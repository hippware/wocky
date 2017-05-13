use Mix.Config

# Configure your database
config :wocky, Wocky.Repo,
  database: "wocky_test",
  pool: Ecto.Adapters.SQL.Sandbox,
  ownership_timeout: 30_000 # ms

config :wocky,
  notification_system: "test",
  indexing_system: "test"

config :logger,
  level: :warn

config :honeybadger,
  environment_name: :test
