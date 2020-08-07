use Mix.Config

# format log timestamps as UTC
config :sasl, :utc_log, true
config :logger, :utc_log, true

# Set the Honeybadger environment
config :honeybadger, environment_name: "${HONEYBADGER_ENV}"

# Log as JSON and turn off ANSI colors
# config :logger, :console,
#   colors: [enabled: false],
#   format: {ExJsonLogger, :format},
#   metadata: :all

# config :libcluster,
#   topologies: [
#     k8s_wocky: [
#       strategy: Cluster.Strategy.Kubernetes.DNS,
#       config: [
#         service: "wocky-private",
#         application_name: "wocky"
#       ]
#     ]
#   ]
