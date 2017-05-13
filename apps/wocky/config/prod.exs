use Mix.Config

# Configure your database
config :wocky, Wocky.Repo,
  adapter: Ecto.Adapters.Postgres,
  database:  {:system, :string, "DB_NAME", "wocky"},
  username:  {:system, :string, "DB_USER", "wocky"},
  password:  {:system, :string, "DB_PASSWORD"},
  hostname:  {:system, :string, "DB_HOST"},
  port:      {:system, :integer, "DB_PORT", 5432},
  pool_size: {:system, :integer, "DB_POOL_SIZE", 15}

config :wocky,
  enable_follow_me_updates: false,
  enable_bot_event_notifications: false,
  async_location_processing: true,
  tros_backend:            Wocky.TROS.S3Store,
  tros_s3_bucket:          {:system, "WOCKY_TROS_S3_BUCKET"},
  application_arn:         {:system, "WOCKY_APPLICATION_ARN"},
  notification_system:     {:system, "WOCKY_NOTIFICATION_SYSTEM"},
  indexing_system:         {:system, "WOCKY_INDEXING_SYSTEM"},
  user_index_name:         {:system, "WOCKY_USER_INDEX_NAME"},
  bot_index_name:          {:system, "WOCKY_BOT_INDEX_NAME"},

config :slackex,
  slack_token: nil

config :honeybadger,
  environment_name: "HONEYBADGER_ENV"
