use Mix.Config

config :wocky_xmpp, wocky_env: 'prod'

config :mnesia, dir: '${HOME}/var/mnesia/${HOSTNAME}'

config :honeybadger, environment_name: "${HONEYBADGER_ENV}"

config :exometer,
  # 5 minutes
  mongooseim_report_interval: 300_000,
  report: [
    reporters: [
      exometer_report_prometheus: [:enable_httpd]
    ]
  ]
