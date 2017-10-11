use Mix.Config

config :wocky,
  enable_bot_event_notifications: false,
  enable_follow_me_updates: false,
  async_location_processing: true,
  tros_backend: Wocky.TROS.S3Store

config :wocky, Wocky.Mailer,
  adapter: {:system, :module, "BAMBOO_ADAPTER", Bamboo.MandrillAdapter},
  api_key: {:system, :string, "MANDRILL_API_KEY"}

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
