use Mix.Config

config :wocky_xmpp, wocky_env: 'dev'

config :honeybadger, environment_name: :dev

config :exometer,
  # 1 minute
  mongooseim_report_interval: 60_000
