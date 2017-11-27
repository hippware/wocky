# This file is responsible for configuring your application
# and its dependencies with the aid of the Mix.Config module.
use Mix.Config

config :wocky,
  ecto_repos: [Wocky.Repo],

  # "Follow me" and location processing
  enable_bot_event_notifications: true,
  enable_follow_me_updates: true,
  async_location_processing: false,

  # TROS file storage in S3
  tros_backend: Wocky.TROS.TestStore,
  tros_s3_bucket:        {:system, "WOCKY_TROS_S3_BUCKET", "wocky-tros-test"},
  tros_s3_region:        {:system, "WOCKY_S3_REGION"},
  tros_s3_access_key_id: {:system, "WOCKY_S3_ACCESS_KEY_ID"},
  tros_s3_secret_key:    {:system, "WOCKY_S3_SECRET_KEY"},

  # FTS indexing (Algolia)
  indexing_system:       {:system, "WOCKY_INDEXING_SYSTEM", "test"},
  user_index_name:       {:system, "WOCKY_USER_INDEX_NAME"},
  bot_index_name:        {:system, "WOCKY_BOT_INDEX_NAME"},

  # Bot reports
  enable_bot_report:     {:system, :boolean, "WOCKY_ENABLE_BOT_REPORT", false},
  bot_report_channel:    {:system, :string,  "WOCKY_BOT_REPORT_CHANNEL", "wocky-reports"},
  bot_report_days:       {:system, :integer, "WOCKY_BOT_REPORT_DAYS", 7},
  slack_token:           {:system, :string,  "SLACK_TOKEN", "xoxb-141728662948-FN75kAhQfnpwil6HbAi5LIQg"},

  # Firebase
  firebase_project_id:   "my-project-1480497595993",

  # Welcome email
  welcome_email_template: "official_tr_welcome_email",
  welcome_email_from:     {"tinyrobot support", "support@tinyrobot.com"},
  welcome_email_subject:  "Welcome to tinyrobot!",
  welcome_field_mappings: [{"user_handle", :handle}]

# Push notifications
config :wocky, Wocky.Push,
  enabled:  {:system, :boolean, "WOCKY_PUSH_ENABLED", false},
  sandbox:  {:system, :boolean, "WOCKY_PUSH_SANDBOX", false}

config :wocky, Wocky.Repo,
  adapter:   Ecto.Adapters.Postgres,
  types:     Wocky.Repo.PostgresTypes,
  database:  {:system, :string, "WOCKY_DB_NAME", "wocky"},
  username:  {:system, :string, "WOCKY_DB_USER", "postgres"},
  password:  {:system, :string, "WOCKY_DB_PASSWORD", "password"},
  hostname:  {:system, :string, "WOCKY_DB_HOST", "localhost"},
  port:      {:system, :integer, "WOCKY_DB_PORT", 5432},
  pool_size: {:system, :integer, "WOCKY_DB_POOL_SIZE", 15},
  loggers: [{ExJsonLogger.Ecto.Logger, :log, [:info]}]

config :wocky, Wocky.Mailer,
  adapter: {:system, :module, "BAMBOO_ADAPTER", Bamboo.TestAdapter},
  api_key: {:system, :string, "MANDRILL_API_KEY", ""}

config :email_checker,
  validations: [EmailChecker.Check.Format]

config :ex_aws,
  access_key_id: [
    {:system, "AWS_ACCESS_KEY_ID"},
    {:awscli, "default", 30},
    :instance_role
  ],
  secret_access_key: [
    {:system, "AWS_SECRET_ACCESS_KEY"},
    {:awscli, "default", 30},
    :instance_role
  ]

config :algolia,
  application_id: "HIE75ZR7Q7"

config :pigeon, :debug_log, true

config :pigeon, :apns,
  apns_default: %{
    cert: {:wocky, "certs/testing.crt"},
    key: {:wocky, "certs/testing.key"},
    mode: :dev
  }

import_config "#{Mix.env}.exs"
