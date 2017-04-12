use Mix.Config

# Configure your database
config :wocky, Wocky.Repo,
  adapter: Ecto.Adapters.MySQL,
  username: "DB_USER",
  password: "DB_PASS",
  database: "wocky_prod",
  pool_size: 15

config :wocky,
  tros_s3_bucket: "wocky-tros-WOCKY_INST",
  tros_s3_access_key_id: "TROS_S3_ACCESS_KEY_ID",
  tros_s3_secret_key: "TROS_S3_SECRET_KEY",
  algolia_user_index_name: "ALGOLIA_USER_INDEX_NAME",
  algolia_bot_index_name: "ALGOLIA_BOT_INDEX_NAME"

config :honeybadger,
  environment_name: "HONEYBADGER_ENV"
