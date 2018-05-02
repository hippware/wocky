defmodule WockyAPI.Router do
  use WockyAPI, :router
  use Honeybadger.Plug

  import WockyAPI.Plugs.Authentication
  import WockyAPI.Plugs.AbsintheConnData

  @max_graphql_complexity 100

  pipeline :rest_api do
    plug :accepts, ["json"]
    plug :check_auth_headers
    plug :ensure_authenticated
    plug :ensure_owner
  end

  scope "/api/v1", WockyAPI do
    pipe_through :rest_api

    resources "/users/:user_id/locations", LocationController, only: [:create]
  end

  pipeline :graphql do
    plug :accepts, ["json"]
    plug :check_auth_headers
    plug :load_graphql_context
  end

  scope "/graphql" do
    pipe_through :graphql

    forward "/", Absinthe.Plug,
      schema: WockyAPI.Schema,
      pipeline: {WockyAPI.Pipeline, :pipeline},
      analyze_complexity: true,
      max_complexity: @max_graphql_complexity
  end

  # Provide a GraphiQL interface in dev mode
  if Mix.env() == :dev do
    forward "/graphiql", Absinthe.Plug.GraphiQL,
      schema: WockyAPI.Schema,
      socket: WockyAPI.UserSocket,
      interface: :playground
  end
end
