use Mix.Config

# Configure your database
config :wocky_db_watcher, Wocky.Repo,
  database: "wocky_test",
  pool: Ecto.Adapters.SQL.Sandbox
