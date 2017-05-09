use Mix.Config

# Configure your database
config :wocky, Wocky.Repo,
  adapter: Ecto.Adapters.Postgres,
  username: "DB_USER",
  password: "DB_PASS",
  database: "wocky_prod",
  pool_size: 15

config :wocky,
  tros_backend: Wocky.TROS.S3,
  async_location_processing: true,
  tros_s3_bucket: "wocky-tros-WOCKY_INST",
  tros_s3_access_key_id: "TROS_S3_ACCESS_KEY_ID",
  tros_s3_secret_key: "TROS_S3_SECRET_KEY",
  application_arn: "APPLICATION_ARN",
  notification_system: "NOTIFICATION_SYSTEM",
  algolia_user_index_name: "ALGOLIA_USER_INDEX_NAME",
  algolia_bot_index_name: "ALGOLIA_BOT_INDEX_NAME"

config :honeybadger,
  environment_name: "HONEYBADGER_ENV"
