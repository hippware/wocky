use Mix.Config

config :wocky,
  indexing_system: "test",
  firebase_load_on_startup: false,
  bot_report_channel: "report-testing"

config :wocky, Wocky.Push,
  enabled: true,
  sandbox: true,
  logging: false

# Configure your database
config :wocky, Wocky.Repo,
  database: "wocky_test",
  pool: Ecto.Adapters.SQL.Sandbox,
  # ms (30 minutes)
  ownership_timeout: 1_800_000

# Make token tests go faster:
config :bcrypt_elixir, bcrypt_log_rounds: 4

# Config for wocky_db_watcher to allow us to run it in tests to test the client
config :wocky_db_watcher, :db,
  database: {:system, :string, "WOCKY_DB_NAME", "wocky_test"},
  username: {:system, :string, "WOCKY_DB_USER", "postgres"},
  password: {:system, :string, "WOCKY_DB_PASSWORD", "password"},
  hostname: {:system, :string, "WOCKY_DB_HOST", "localhost"},
  port: {:system, :integer, "WOCKY_DB_PORT", 5432},
  pool_size: {:system, :integer, "WOCKY_DB_POOL_SIZE", 15},
  pool: Ecto.Adapters.SQL.Sandbox
