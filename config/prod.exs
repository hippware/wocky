use Mix.Config

config :exometer,
  wocky_report_interval: 60000, ## 60 seconds
  report: [
    {:reporters, [
      {:exometer_report_graphite, [
        {:prefix, 'wocky'},
        {:connect_timeout, 5000},
        {:host, 'metrics1.staging.dev.tinyrobot.com'},
        {:port, 2003},
        {:api_key, ''}
      ]}
    ]}
  ]
