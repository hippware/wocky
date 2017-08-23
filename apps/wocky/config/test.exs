use Mix.Config

# Configure your database
config :wocky, Wocky.Repo,
  database: "wocky_test",
  pool: Ecto.Adapters.SQL.Sandbox,
  ownership_timeout: 30_000 # ms

config :wocky,
  async_push_notifications: false,
  enable_push_notifications: true,
  indexing_system: "test",
  firebase_load_on_startup: false,
  firebase_project_id: "tinyrobot-3ce47"

config :logger,
  level: :warn

# Make token tests go faster:
config :comeonin,
  bcrypt_log_rounds: 4

config :pushex,
  sandbox: true
