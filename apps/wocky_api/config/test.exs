use Mix.Config

config :wocky_api,
  location_api_port: 4000

# We don't run a server during test. If one is required,
# you can enable the server option below.
config :wocky_api, WockyAPI.Endpoint,
  http: [port: 4001],
  server: false
