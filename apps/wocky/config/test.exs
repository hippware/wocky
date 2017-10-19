use Mix.Config

# Configure your database
config :wocky, Wocky.Repo,
  database: "wocky_test",
  pool: Ecto.Adapters.SQL.Sandbox,
  ownership_timeout: 30_000 # ms

config :wocky, Wocky.Mailer,
  adapter: Bamboo.TestAdapter

config :wocky,
  async_push_notifications: false,
  enable_push_notifications: true,
  indexing_system: "test",
  firebase_load_on_startup: false,
  bot_report_channel: "report-testing"

config :logger,
  level: :warn

# Make token tests go faster:
config :bcrypt_elixir,
  bcrypt_log_rounds: 4

config :pushex,
  sandbox: true
