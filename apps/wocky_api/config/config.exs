# This file is responsible for configuring your application
# and its dependencies with the aid of the Mix.Config module.
use Mix.Config

# General application configuration
config :wocky_api,
  namespace: WockyAPI,
  ecto_repos: [Wocky.Repo]

config :wocky_api, :generators,
  context_app: :wocky,
  binary_id: true

# Configures the endpoint
config :wocky_api, WockyAPI.Endpoint,
  url: [host: "localhost"],
  secret_key_base: "teo9ScPXCxIsZm9KWkEsAub4XqnAhp7FvQLGCVe9f3Bmvn9iyzt5Jkz/ZtxPUY8F",
  render_errors: [view: WockyAPI.ErrorView, accepts: ~w(json)],
  pubsub: [name: WockyAPI.PubSub,
           adapter: Phoenix.PubSub.PG2]

# Import environment specific config. This must remain at the bottom
# of this file so it overrides the configuration defined above.
import_config "#{Mix.env}.exs"
