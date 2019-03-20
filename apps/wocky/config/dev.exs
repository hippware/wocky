use Mix.Config

config :wocky,
  start_watcher: true

config :wocky, Wocky.User.GeoFence,
  enter_debounce_seconds: 0,
  exit_debounce_seconds: 0

config :wocky, Wocky.Repo,
  database: "wocky_dev",
  pool_size: 10

config :exometer_core,
  report: [reporters: [{:exometer_report_prometheus, [:enable_httpd]}]]

config :elixometer,
  reporter: :exometer_report_prometheus,
  env: Mix.env()
