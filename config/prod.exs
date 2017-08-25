use Mix.Config

# Do not print debug messages in production
config :logger, level: :info

config :libcluster,
  topologies: [
    wocky_k8s: [
      strategy: Cluster.Strategy.Kubernetes,
      config: [
        kubernetes_selector: "app=wocky",
        kubernetes_node_basename: "wocky"
      ]
    ]
  ]
