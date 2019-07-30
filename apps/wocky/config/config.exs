# This file is responsible for configuring your application
# and its dependencies with the aid of the Mix.Config module.
use Mix.Config

config :wocky,
  ecto_repos: [Wocky.Repo],
  db_only_mode: false,
  start_watcher: {:system, :boolean, "WOCKY_START_WATCHER", true},

  # Deployment notifications
  deploy_notify_channels: ["#dev-deployments"],

  # TROS file storage in test storage system
  tros_backend: {:system, :module, "WOCKY_TROS_STORE", Wocky.TROS.Store.Test},
  tros_s3_bucket: {:system, "WOCKY_TROS_S3_BUCKET", "wocky-tros-test"},
  tros_s3_region: {:system, "WOCKY_S3_REGION", "us-west-2"},
  tros_s3_server: {:system, "WOCKY_S3_SERVER", "s3.amazonaws.com"},

  # Deployment notifications
  slack_token: {:system, :string, "SLACK_TOKEN"},

  # Authentication
  token_expiry_days: {:system, :integer, "WOCKY_TOKEN_EXPIRY_DAYS", 60},
  enable_auth_bypass: {:system, :boolean, "WOCKY_ENABLE_BYPASS", true},
  auth_bypass_prefixes: ["+1555", "+1556"],
  client_jwt_signing_key:
    {:system, "WOCKY_CLIENT_JWT_SIGNING_KEY",
     "CgKG3D0OfVBMh3JiJfQGkS0SyTrBaaGfrl1MozWnjesSuhVLnMTHDwyXDC/f2dtu"},
  server_jwt_signing_key:
    {:system, "WOCKY_SERVER_JWT_SIGNING_KEY",
     "+K+XxznYgxCGLa5hZo9Qyb7QtpmmRPOgNXM4UYfKViYnuiIjTySItwSk7rH+Uv2g"},

  # Permitted agents
  permitted_agents: ["TinyRobot", "Overseer"],

  # Welcome email
  send_welcome_email: {:system, :boolean, "SEND_WELCOME_EMAIL", false},
  welcome_email_template: "official_tr_welcome_email",
  welcome_email_from: {"tinyrobot support", "support@tinyrobot.com"},
  welcome_email_subject: "Welcome to tinyrobot!",
  welcome_field_mappings: [{"user_handle", :handle}],

  # SMS messaging
  sms_backend: {:system, :module, "WOCKY_SMS_BACKEND", Wocky.SMS.Sandbox},
  twilio_number: "+12133401134",
  max_sms_per_user: {:system, :integer, "WOCKY_MAX_SMS_PER_USER", 100},
  # Overseer test user
  unlimited_sms_numbers: ["+15556667777"],

  # Country code
  country_code_lookup_method:
    {:system, :atom, "WOCKY_CC_LOOKUP_METHOD", :hardwire},
  country_code_hardwire_value:
    {:system, :string, "WOCKY_CC_HARDWIRE_VAL", "US"},

  # Dynamic links
  dynamic_link_backend:
    {:system, :module, "WOCKY_DYN_LINK_BACKEND", Wocky.DynamicLink.Sandbox},
  app_store_id: {:system, :string, "WOCKY_APP_STORE_ID", "1295678402"},
  ios_bundle_id:
    {:system, :string, "WOCKY_IOS_BUNDLE_ID", "com.hippware.tinyrobot"},
  android_package_name:
    {:system, :string, "WOCKY_ANDROID_PACKAGE_NAME",
     "com.hippware.android.tinyrobot"},
  android_fallback_link: "https://tinyrobot.com",
  firebase_domain_url_prefix: "https://tinyrobot.page.link",
  firebase_link_prefix: "https://tinyrobot.com/?inviteCode=",

  # Goth config
  goth_config: %{
    type: "service_account",
    project_id: "my-project-1480497595993",
    private_key_id: "b9d64bc1a6d8edda824eb2ab984c8238701818ea",
    client_email:
      "firebase-adminsdk-xrj66@my-project-1480497595993.iam.gserviceaccount.com",
    client_id: "107308386875224786877",
    auth_uri: "https://accounts.google.com/o/oauth2/auth",
    token_uri: "https://oauth2.googleapis.com/token",
    auth_provider_x509_cert_url: "https://www.googleapis.com/oauth2/v1/certs",
    client_x509_cert_url:
      "https://www.googleapis.com/robot/v1/metadata/x509/firebase-adminsdk-xrj66%40my-project-1480497595993.iam.gserviceaccount.com"
  },
  goth_private_key: {:system, :string, "FIREBASE_PRIVATE_KEY", "dummy_key"},

  # User cleanup
  expire_transient_users_after_days:
    {:system, :integer, "WOCKY_EXPIRE_TRANSIENT_USERS_AFTER_DAYS", nil},

  # Bot search
  max_local_bots_search_radius:
    {:system, :integer, "WOCKY_MAX_LOCAL_BOTS_SEARCH_RADIUS", 50_000},


  # Unused value to test the Vault adapter - can be safely deleted
  vault_value: {{:via, Wocky.ConfexVaultAdapter}, "test-secret-value"}

config :wocky, :redis,
  host: {:system, :string, "REDIS_HOST", "localhost"},
  port: {:system, :integer, "REDIS_PORT", 6379},
  ssl: {:system, :boolean, "REDIS_SSL", false},
  password: {:system, :string, "REDIS_PASSWORD", nil},
  database: {:system, :integer, "REDIS_DB", 0}

config :wocky, :redlock,
  pool_size: 2,
  drift_factor: 0.01,
  max_retry: 10,
  retry_interval_base: 300,
  retry_interval_max: 3_000,
  reconnection_interval_base: 500,
  reconnection_interval_max: 5_000,
  servers: [
    [
      host: {:system, :string, "REDIS_HOST", "localhost"},
      port: {:system, :integer, "REDIS_PORT", 6379},
      ssl: {:system, :boolean, "REDIS_SSL", false},
      auth: {:system, :string, "REDIS_PASSWORD", nil},
      database: {:system, :integer, "REDIS_DB", 0}
    ]
  ]

config :wocky, :pigeon,
  apns: [
    cert: {:wocky, "certs/testing.crt"},
    key: {:wocky, "certs/testing.key"},
    mode: :dev
  ],
  fcm: [
    key: {:system, :string, "WOCKY_FCM_KEY", ""}
  ]

config :wocky, Wocky.Audit,
  log_traffic: {:system, :boolean, "WOCKY_AUDIT_LOG_TRAFFIC", true},
  log_location: {:system, :boolean, "WOCKY_AUDIT_LOG_LOCATION", true},
  log_push: {:system, :boolean, "WOCKY_AUDIT_LOG_PUSH", true},
  log_push_payload: {:system, :boolean, "WOCKY_AUDIT_LOG_PUSH_PAYLOAD", true}

# location processing
config :wocky, Wocky.Location.GeoFence,
  enable_notifications: true,
  debounce: true,
  enter_debounce_seconds:
    {:system, :integer, "WOCKY_ENTER_DEBOUNCE_SECONDS", 30},
  exit_debounce_seconds: {:system, :integer, "WOCKY_EXIT_DEBOUNCE_SECONDS", 30},
  max_accuracy_threshold:
    {:system, :integer, "WOCKY_GEOFENCE_MAX_ACCURACY_THRESHOLD", 50},
  max_slow_speed: {:system, :integer, "WOCKY_GEOFENCE_MAX_SLOW_SPEED", 2},
  max_exit_distance:
    {:system, :integer, "WOCKY_GEOFENCE_MAX_EXIT_DISTANCE", 200},
  stale_update_seconds:
    {:system, :integer, "WOCKY_GEOFENCE_STALE_UPDATE_SECONDS", 300}

# Push notifications
config :wocky, Wocky.Notifier.Push,
  enabled: {:system, :boolean, "WOCKY_PUSH_ENABLED", false},
  sandbox: {:system, :boolean, "WOCKY_PUSH_SANDBOX", false},
  uri_prefix: {:system, :string, "WOCKY_PUSH_URI_PREFIX", "tinyrobot"},
  timeout: {:system, :integer, "WOCKY_PUSH_TIMEOUT", 60_000}

config :wocky, Wocky.Notifier.Push.Backend.APNS,
  topic: {:system, :string, "WOCKY_PUSH_APNS_TOPIC", "app"}

config :wocky, Wocky.Notifier.Push.Backend.FCM,
  package: {:system, :string, "WOCKY_PUSH_FCM_PACKAGE", "app"}

config :wocky, Wocky.Notifier.Push.Backend.Sandbox,
  reflect: {:system, :boolean, "WOCKY_PUSH_REFLECT", false}

config :wocky, Wocky.Repo,
  adapter: Ecto.Adapters.Postgres,
  types: Wocky.Repo.PostgresTypes,
  database: {:system, :string, "WOCKY_DB_NAME", "wocky"},
  username: {:system, :string, "WOCKY_DB_USER", "postgres"},
  password: {:system, :string, "WOCKY_DB_PASSWORD", "password"},
  hostname: {:system, :string, "WOCKY_DB_HOST", "localhost"},
  port: {:system, :integer, "WOCKY_DB_PORT", 5432},
  pool_size: {:system, :integer, "WOCKY_DB_POOL_SIZE", 15},
  migration_timestamps: [type: :utc_datetime_usec]

config :wocky, Wocky.Notifier.Email.Mailer,
  adapter: {:system, :module, "BAMBOO_ADAPTER", Bamboo.TestAdapter},
  api_key: {:system, :string, "MANDRILL_API_KEY", ""}

config :wocky, Wocky.ConfexVaultAdapter,
  use_vault: false
  vault_prefix: {:system, :string, "WOCKY_VAULT_PREFIX", ""}

config :dawdle, start_pollers: true

config :dawdle_db, channel: "wocky_db_watcher_notify"

config :email_checker, validations: [EmailChecker.Check.Format]

config :ex_aws,
  access_key_id: [
    {:system, "AWS_ACCESS_KEY_ID"},
    {:awscli, "default", 30}
  ],
  secret_access_key: [
    {:system, "AWS_SECRET_ACCESS_KEY"},
    {:awscli, "default", 30}
  ]

config :pigeon,
  debug_log: true,
  workers: [
    {Wocky.PigeonConfig, :apns_config},
    {Wocky.PigeonConfig, :fcm_config}
  ]

config :ex_twilio,
  account_sid: {:system, "TWILIO_ACCOUNT_SID"},
  auth_token: {:system, "TWILIO_AUTH_TOKEN"}

config :goth, config_module: Wocky.GothConfig

config :fun_with_flags, :redis,
  host: {:system, :string, "REDIS_HOST", "localhost"},
  port: {:system, :integer, "REDIS_PORT", 6379},
  ssl: {:system, :boolean, "REDIS_SSL", false},
  password: {:system, :string, "REDIS_PASSWORD", nil},
  database: {:system, :integer, "REDIS_DB", 0}

config :vaultex,
  vault_addr: "http://vault-vault.vault:8200"

import_config "#{Mix.env()}.exs"
