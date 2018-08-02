use Mix.Config

config :wocky,
  tros_backend: {:system, :module, "WOCKY_TROS_STORE", Wocky.TROS.S3Store},
  start_watcher: {:system, :boolean, "WOCKY_START_WATCHER", false}

config :wocky, Wocky.User.GeoFence,
  async_processing: true,
  visit_timeout_enabled: false

config :wocky_db_watcher,
  backend:
    {:system, :module, "WOCKY_DB_WATCHER_BACKEND", WockyDBWatcher.Backend.SQS}

config :wocky_db_watcher, WockyDBWatcher.Backend.SQS,
  region: {:system, :string, "WOCKY_DB_WATCHER_REGION", "us-west-2"},
  queue: {:system, :string, "WOCKY_DB_WATCHER_QUEUE"}

config :pigeon, :apns,
  apns_default: %{
    cert: {:wocky, "certs/${WOCKY_INST}.crt"},
    key: {:wocky, "certs/${WOCKY_INST}.key"},
    mode: :prod
  }

config :dawdle,
  backend: {:system, :module, "DAWDLE_BACKEND", Dawdle.Backend.SQS}

config :dawdle, Dawdle.Backend.SQS,
  region: {:system, :string, "DAWDLE_REGION", "us-west-2"},
  queues: {:system, :list, "DAWDLE_QUEUES"}

# The items below are only used in local dev environments - in AWS environments
# they are ignored since wocky_db_watcher runs in its own container
config :wocky_db_watcher, :db,
  database: {:system, :string, "WOCKY_DB_NAME"},
  username: {:system, :string, "WOCKY_DB_USER"},
  password: {:system, :string, "WOCKY_DB_PASSWORD"},
  hostname: {:system, :string, "WOCKY_DB_HOST"}

config :exometer_core,
  report: [reporters: [{:exometer_report_prometheus, [:enable_httpd]}]]

config :elixometer,
  reporter: :exometer_report_prometheus,
  metric_prefix: "wocky",
  env: "${WOCKY_INST}"
