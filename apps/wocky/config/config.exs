# This file is responsible for configuring your application
# and its dependencies with the aid of the Mix.Config module.
use Mix.Config

config :wocky,
  ecto_repos: [Wocky.Repo],
  enable_bot_event_notifications: true,
  enable_follow_me_updates: true,
  async_location_processing: false,
  tros_backend: Wocky.TROS.TestStore,
  tros_s3_bucket:        {:system, "WOCKY_TROS_S3_BUCKET", "wocky-tros-test"},
  tros_s3_region:        {:system, "WOCKY_S3_REGION"},
  tros_s3_access_key_id: {:system, "WOCKY_S3_ACCESS_KEY_ID"},
  tros_s3_secret_key:    {:system, "WOCKY_S3_SECRET_KEY"},
  indexing_system:       {:system, "WOCKY_INDEXING_SYSTEM", "test"},
  user_index_name:       {:system, "WOCKY_USER_INDEX_NAME"},
  bot_index_name:        {:system, "WOCKY_BOT_INDEX_NAME"},
  enable_bot_report:     {:system, :boolean, "WOCKY_ENABLE_BOT_REPORT", false},
  enable_push_notifications: {:system, :boolean, "WOCKY_ENABLE_PUSH", false},
  firebase_project_id:   "my-project-1480497595993",

  welcome_email_template: "official_tr_welcome_email",
  welcome_email_from:     {"tinyrobot support", "support@tinyrobot.com"},
  welcome_email_subject:  "Welcome to tinyrobot!",
  welcome_field_mappings: [{"user_handle", :handle}]

config :wocky, Wocky.Repo,
  adapter:   Ecto.Adapters.Postgres,
  types:     Wocky.Repo.PostgresTypes,
  database:  {:system, :string, "WOCKY_DB_NAME", "wocky"},
  username:  {:system, :string, "WOCKY_DB_USER", "postgres"},
  password:  {:system, :string, "WOCKY_DB_PASSWORD", "password"},
  hostname:  {:system, :string, "WOCKY_DB_HOST", "localhost"},
  port:      {:system, :integer, "WOCKY_DB_PORT", 5432},
  pool_size: {:system, :integer, "WOCKY_DB_POOL_SIZE", 15}

config :logger,
  level: :info

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

config :slackex,
  token: "xoxb-141728662948-FN75kAhQfnpwil6HbAi5LIQg"

config :algolia,
  application_id: "HIE75ZR7Q7"

config :apns,
  pools: [],
  callback_module: Pushex.APNS.Callback

config :pushex,
  event_handlers: [Wocky.PushEventHandler],
  apns: [
    default_app: "testing",
    apps: [
      [
        name: "testing",
        env: :prod,
        certfile: {:wocky, "certs/testing.crt"},
        keyfile: {:wocky, "certs/testing.key"},
        pool_size: 5
      ]
    ]
  ]

import_config "#{Mix.env}.exs"
