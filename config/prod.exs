use Mix.Config

# format log timestamps as UTC
config :sasl, :utc_log, true
config :logger, :utc_log, true

# Log as JSON and turn off ANSI colors
config :logger, :console,
  colors: [enabled: false],
  format: {ExJsonLogger, :format},
  metadata: :all

config :peerage, via: Peerage.Via.Dns,
  dns_name: "wocky-private.${KUBE_POD_NS}.svc.cluster.local",
  app_name: "wocky",
  interval: 5
