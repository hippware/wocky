use Mix.Config

alias Wocky.Config.VaultAdapter

config :wocky,
  tros_backend: {:system, :module, "WOCKY_TROS_STORE", Wocky.TROS.Store.S3},
  dynamic_link_backend:
    {:system, :module, "WOCKY_DYN_LINK_BACKEND", Wocky.DynamicLink.Firebase},
  country_code_lookup_method:
    {:system, :atom, "WOCKY_CC_LOOKUP_METHOD", :twilio},
  goth_private_key: {{:via, VaultAdapter}, "firebase-private-key"},
  slack_token: {{:via, VaultAdapter}, "slack-token"},
  client_jwt_signing_key: {{:via, VaultAdapter}, "client-jwt-signing-key"},
  server_jwt_signing_key: {{:via, VaultAdapter}, "server-jwt-signing-key"}

config :wocky, Wocky.Repo, password: {{:via, VaultAdapter}, "db-password"}

config :wocky, Wocky.Location.GeoFence, visit_timeout_enabled: false

config :wocky, Wocky.Notifier.Email.Mailer,
  api_key: {{:via, VaultAdapter}, "mandrill-api-key", ""}

config :wocky, :pigeon,
  apns: [
    cert: {:wocky, "certs/${WOCKY_INST}.crt"},
    key: {:wocky, "certs/${WOCKY_INST}.key"},
    mode: :prod
  ],
  fcm: [
    key: {{:via, VaultAdapter}, "fcm-key"}
  ]

config :dawdle, backend: Dawdle.Backend.SQS

config :exometer_core,
  report: [reporters: [{:exometer_report_prometheus, [:enable_httpd]}]]

config :elixometer,
  reporter: :exometer_report_prometheus,
  metric_prefix: "wocky",
  env: "${WOCKY_INST}"

config :ex_aws,
  access_key_id: :instance_role,
  secret_access_key: :instance_role

config :wocky, :redis, password: {{:via, VaultAdapter}, "redis-password", nil}

config :wocky, :redlock,
  servers: [
    [
      host: {:system, :string, "REDIS_HOST", "localhost"},
      port: {:system, :integer, "REDIS_PORT", 6379},
      ssl: {:system, :boolean, "REDIS_SSL", false},
      auth: {{:via, VaultAdapter}, "redis-password", nil},
      database: {:system, :integer, "REDIS_DB", 0}
    ]
  ]

config :fun_with_flags, :redis,
  password: {{:via, VaultAdapter}, "redis-password", nil}

config :honeybadger,
  api_key: {{:via, VaultAdapter}, "honeybadger-api-key"}
