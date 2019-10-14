use Mix.Config

config :wocky_api,
  allow_factory_insert: true,
  enable_location_request_trigger: true

config :wocky_api, WockyAPI.Endpoint,
  http: [port: 4001],
  server: true

config :wocky_api, WockyAPI.Test.Context,
  client: WockyAPI.Test.Client,
  query_caller: CommonGraphQLClient.Caller.WebSocket,
  subscription_caller: CommonGraphQLClient.Caller.WebSocket,
  websocket_api_url: "ws://127.0.0.1:4001/graphql/websocket"
