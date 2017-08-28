use Mix.Config

# Do not print debug messages in production
config :logger, level: :info

config :peerage, via: Peerage.Via.Dns,
  dns_name: "wocky-private.${KUBE_POD_NS}.svc.cluster.local",
  app_name: "wocky",
  interval: 5
