# This file is responsible for configuring your application
# and its dependencies with the aid of the Mix.Config module.
use Mix.Config

config :wocky,
  ecto_repos: [Wocky.Repo],

  # TROS file storage in test storage system
  tros_backend: {:system, :module, "WOCKY_TROS_STORE", Wocky.TROS.TestStore},
  tros_s3_bucket: {:system, "WOCKY_TROS_S3_BUCKET", "wocky-tros-test"},
  tros_s3_region: {:system, "WOCKY_S3_REGION", "us-west-2"},
  tros_s3_server: {:system, "WOCKY_S3_SERVER", "s3.amazonaws.com"},
  tros_s3_access_key_id: {:system, "AWS_ACCESS_KEY_ID"},
  tros_s3_secret_key: {:system, "AWS_SECRET_ACCESS_KEY"},

  # Deployment notifications
  slack_token: {:system, :string, "SLACK_TOKEN"},

  # Authentication
  token_expiry_days: {:system, :integer, "WOCKY_TOKEN_EXPIRY_DAYS", 60},
  enable_auth_bypass: {:system, :boolean, "WOCKY_ENABLE_BYPASS", true},
  auth_bypass_prefixes: ["+1555"],

  # Welcome email
  send_welcome_email: {:system, :boolean, "SEND_WELCOME_EMAIL", false},
  welcome_email_template: "official_tr_welcome_email",
  welcome_email_from: {"tinyrobot support", "support@tinyrobot.com"},
  welcome_email_subject: "Welcome to tinyrobot!",
  welcome_field_mappings: [{"user_handle", :handle}]

config :wocky, :redis,
  host: {:system, :string, "REDIS_HOST", "localhost"},
  port: {:system, :integer, "REDIS_PORT", 6379},
  db: {:system, :integer, "REDIS_DB", 0}

# location processing
config :wocky, Wocky.User.GeoFence,
  enable_notifications: true,
  async_processing: false,
  debounce: true,
  enter_debounce_seconds:
    {:system, :integer, "WOCKY_ENTER_DEBOUNCE_SECONDS", 30},
  exit_debounce_seconds: {:system, :integer, "WOCKY_EXIT_DEBOUNCE_SECONDS", 30},
  visit_timeout_seconds:
    {:system, :integer, "WOCKY_VISIT_TIMEOUT_SECONDS", 1_800},
  visit_timeout_enabled:
    {:system, :boolean, "WOCKY_VISIT_TIMEOUT_ENABLED", true},
  max_accuracy_threshold:
    {:system, :integer, "WOCKY_GEOFENCE_MAX_ACCURACY_THRESHOLD", 90},
  max_slow_speed: {:system, :integer, "WOCKY_GEOFENCE_MAX_SLOW_SPEED", 2},
  max_exit_distance:
    {:system, :integer, "WOCKY_GEOFENCE_MAX_EXIT_DISTANCE", 200},
  stale_update_seconds:
    {:system, :integer, "WOCKY_GEOFENCE_STALE_UPDATE_SECONDS", 300}

# Push notifications
config :wocky, Wocky.Push,
  enabled: {:system, :boolean, "WOCKY_PUSH_ENABLED", false},
  sandbox: {:system, :boolean, "WOCKY_PUSH_SANDBOX", false},
  reflect: {:system, :boolean, "WOCKY_PUSH_REFLECT", false},
  topic: {:system, :string, "WOCKY_PUSH_TOPIC", "app"},
  uri_prefix: {:system, :string, "WOCKY_PUSH_URI_PREFIX", "tinyrobot"},
  timeout: {:system, :integer, "WOCKY_PUSH_TIMEOUT", 60_000}

config :wocky, Wocky.Repo,
  adapter: Ecto.Adapters.Postgres,
  types: Wocky.Repo.PostgresTypes,
  database: {:system, :string, "WOCKY_DB_NAME", "wocky"},
  username: {:system, :string, "WOCKY_DB_USER", "postgres"},
  password: {:system, :string, "WOCKY_DB_PASSWORD", "password"},
  hostname: {:system, :string, "WOCKY_DB_HOST", "localhost"},
  port: {:system, :integer, "WOCKY_DB_PORT", 5432},
  pool_size: {:system, :integer, "WOCKY_DB_POOL_SIZE", 15},
  loggers: [{ExJsonLogger.Ecto.Logger, :log, [:debug]}, Wocky.Repo.Instrumenter]

config :wocky, Wocky.Mailer,
  adapter: {:system, :module, "BAMBOO_ADAPTER", Bamboo.TestAdapter},
  api_key: {:system, :string, "MANDRILL_API_KEY", ""}

config :wocky_db_watcher,
  backend: WockyDBWatcher.Backend.Direct,
  channel: "wocky_db_watcher_notify"

config :email_checker, validations: [EmailChecker.Check.Format]

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

config :pigeon, :debug_log, true

config :pigeon, :apns,
  apns_default: %{
    cert: {:wocky, "certs/testing.crt"},
    key: {:wocky, "certs/testing.key"},
    mode: :dev
  }

import_config "#{Mix.env()}.exs"
