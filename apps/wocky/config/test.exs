use Mix.Config

# Configure your database
config :wocky, Wocky.Repo,
  adapter: Ecto.Adapters.MySQL,
  username: "root",
  password: "",
  database: "wocky_test",
  hostname: "localhost",
  pool: Ecto.Adapters.SQL.Sandbox

config :wocky,
  location_api_port: 3000,
  enable_bot_event_notifications: true,
  enable_follow_me_updates: true

config :logger,
  level: :warn

config :honeybadger,
  environment_name: "Testing"
