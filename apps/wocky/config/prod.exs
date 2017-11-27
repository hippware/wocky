use Mix.Config

config :wocky,
  enable_bot_event_notifications: false,
  enable_follow_me_updates: false,
  async_location_processing: true,
  tros_backend: Wocky.TROS.S3Store

config :wocky, Wocky.Mailer,
  adapter: {:system, :module, "BAMBOO_ADAPTER", Bamboo.MandrillAdapter},
  api_key: {:system, :string, "MANDRILL_API_KEY"}

config :pigeon, :apns,
  apns_default: %{
    cert: {:wocky, "certs/${WOCKY_INST}.crt"},
    key: {:wocky, "certs/${WOCKY_INST}.key"},
    mode: :prod
  }
