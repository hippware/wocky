use Mix.Config

config :wocky,
  enable_bot_event_notifications: false,
  enable_follow_me_updates: false,
  async_location_processing: true,
  tros_backend: Wocky.TROS.S3Store

config :slackex,
  token: nil

config :quantum,
  global?: true

config :quantum, :wocky,
  cron: [
    bot_report: [
      schedule: "@weekly",
      task: "Wocky.BotReport.run",
      args: ["wocky-reports", 7]
    ]
  ]
