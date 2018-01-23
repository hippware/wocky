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
