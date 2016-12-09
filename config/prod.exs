use Mix.Config

config :mnesia,
  dir: 'data/mnesia'

config :lager,
  log_root: 'log'

config :exometer,
  wocky_report_interval: 60000, ## 60 seconds
  report: [
    {:reporters, [
      {:exometer_report_graphite, [
        {:prefix, 'wocky'},
        {:connect_timeout, 5000},
        {:host, 'metrics1.staging.dev.tinyrobot.com'},
        {:port, 2003},
        {:api_key, 'GRAPHITE_KEY'}
      ]}
    ]}
  ]

config :honeybadger,
  environment_name: "HONEYBADGER_ENV"
