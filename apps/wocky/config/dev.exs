use Mix.Config

# Configure your database
config :wocky, Wocky.Repo,
  adapter: Ecto.Adapters.MySQL,
  username: "root",
  password: "",
  database: "wocky_dev",
  hostname: "localhost",
  pool_size: 10

config :wocky,
  tros_s3_bucket: "wocky-tros-test",
  tros_s3_access_key_id: "AKIAJED6VHHVIE2ZM5CA",
  tros_s3_secret_key: "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",
  enable_bot_event_notifications: true,
  enable_follow_me_updates: true

config :honeybadger,
  environment_name: "Development"
