use Mix.Config

config :wocky,
  enable_bot_event_notifications: false,
  enable_follow_me_updates: false,
  async_location_processing: true,
  tros_backend: Wocky.TROS.S3Store,
  firebase_project_id: "tinyrobot-3ce47"

config :slackex,
  token: nil

config :pushex,
  apns: [
    default_app: "${WOCKY_INST}",
    apps: [
      [
        name: "${WOCKY_INST}",
        env: :prod,
        certfile: {:wocky, "certs/${WOCKY_INST}.crt"},
        keyfile: {:wocky, "certs/${WOCKY_INST}.key"},
        pool_size: 5
      ]
    ]
  ]

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
