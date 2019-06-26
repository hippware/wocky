use Mix.Config

config :wocky,
  tros_backend: {:system, :module, "WOCKY_TROS_STORE", Wocky.TROS.Store.S3},
  dynamic_link_backend:
    {:system, :module, "WOCKY_DYN_LINK_BACKEND", Wocky.DynamicLink.Firebase},
  country_code_lookup_method:
    {:system, :atom, "WOCKY_CC_LOOKUP_METHOD", :twilio}

config :wocky, Wocky.Location.GeoFence, visit_timeout_enabled: false

config :wocky, :pigeon,
  apns: [
    cert: {:wocky, "certs/${WOCKY_INST}.crt"},
    key: {:wocky, "certs/${WOCKY_INST}.key"},
    mode: :prod
  ]

config :dawdle, backend: Dawdle.Backend.SQS

config :exometer_core,
  report: [reporters: [{:exometer_report_prometheus, [:enable_httpd]}]]

config :elixometer,
  reporter: :exometer_report_prometheus,
  metric_prefix: "wocky",
  env: "${WOCKY_INST}"
