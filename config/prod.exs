use Mix.Config

# Do not print debug messages in production
config :logger, level: :info

config :peerage, via: Peerage.Via.Dns,
  dns_name: "wocky-service-headless.default.svc.cluster.local",
  app_name: "wocky",
  interval: 5
