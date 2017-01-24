use Mix.Config

config :wocky,
  wocky_env: 'prod'

config :mnesia,
  dir: 'data/mnesia'

config :lager,
  log_root: 'log'

config :exometer,
  mongooseim_report_interval: 300000, # 5 minutes
  report: [
    {:reporters, [
      {:exometer_report_cloudwatch, [
        {:access_key_id, 'CLOUDWATCH_KEY_ID'},
        {:secret_access_key, 'CLOUDWATCH_SECRET_KEY'},
        {:region, 'CLOUDWATCH_REGION'},
        {:namespace, 'App/Wocky'},
        {:dimensions, [{'InstanceId', 'CLOUDWATCH_ID'}]}
      ]}
    ]}
  ]

config :honeybadger,
  environment_name: "HONEYBADGER_ENV"
