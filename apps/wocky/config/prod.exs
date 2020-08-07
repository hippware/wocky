use Mix.Config

config :wocky,
  tros_backend: {:system, :module, "WOCKY_TROS_STORE", Wocky.TROS.Store.S3},
  dynamic_link_backend:
    {:system, :module, "WOCKY_DYN_LINK_BACKEND",
     Wocky.UserInvite.DynamicLink.Firebase},
  country_code_lookup_method:
    {:system, :atom, "WOCKY_CC_LOOKUP_METHOD", :twilio},
  location_share_end_self: true

config :wocky, Wocky.Location.GeoFence, visit_timeout_enabled: false

config :wocky, :pigeon,
  apns: [
    key_identifier: {:system, :string, "APNS_KEY_IDENTIFIER", "NBJ9A4785H"},
    team_id: {:system, :string, "APNS_TEAM_ID", "W6M2PMRSBT"},
    mode: :prod
  ]

# config :dawdle, backend: Dawdle.Backend.SQS

config :exometer_core,
  report: [reporters: [{:exometer_report_prometheus, [:enable_httpd]}]]

config :elixometer,
  env: "${WOCKY_INST}"

# config :ex_aws,
#   access_key_id: :instance_role,
#   secret_access_key: :instance_role

config :honeybadger,
  breadcrumbs_enabled: true
