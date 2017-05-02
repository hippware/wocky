use Mix.Config

# Configure your database
config :wocky, Wocky.Repo,
  adapter: Ecto.Adapters.Postgres,
  username: "postgres",
  password: "password",
  database: "wocky_test",
  hostname: "localhost",
  pool: Ecto.Adapters.SQL.Sandbox

config :wocky,
  tros_s3_bucket: "wocky-tros-test",
  tros_s3_access_key_id: "AKIAJED6VHHVIE2ZM5CA",
  tros_s3_secret_key: "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",
  enable_bot_event_notifications: true,
  enable_follow_me_updates: true

config :logger,
  level: :warn

config :honeybadger,
  environment_name: "Testing"
