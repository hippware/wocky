use Mix.Config

config :wocky,
  enable_bot_report: true,
  bot_report_channel: "report-testing"

config :wocky, Wocky.Repo,
  database: "wocky_dev",
  pool_size: 10
