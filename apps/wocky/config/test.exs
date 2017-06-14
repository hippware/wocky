use Mix.Config

# Configure your database
config :wocky, Wocky.Repo,
  database: "wocky_test",
  pool: Ecto.Adapters.SQL.Sandbox,
  ownership_timeout: 30_000 # ms

config :wocky,
  enable_push_notifications: true,
  indexing_system: "test"

config :logger,
  level: :warn

# Make token tests go faster:
config :comeonin,
  bcrypt_log_rounds: 4

config :pushex,
  sandbox: true
