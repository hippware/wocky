defmodule WockyAPI.Router do
  use WockyAPI, :router
  use Honeybadger.Plug

  import WockyAPI.Plugs.Authentication
  import WockyAPI.Plugs.AbsintheConnData

  @max_graphql_complexity Application.fetch_env!(
                            :wocky_api,
                            :max_graphql_complexity
                          )

  pipeline :rest_api do
    plug :accepts, ["json"]
    plug :check_location_auth
    plug :ensure_authenticated
    plug :ensure_owner
  end

  scope "/api/v1", WockyAPI.Controllers do
    pipe_through :rest_api

    resources "/users/:user_id/locations", LocationController, only: [:create]

    put "/safety/geometries/:source/:source_id", GeometriesController, :create

    put "/safety/alerts/:source/import", AlertsController, :start_import
    delete "/safety/alerts/:source/import", AlertsController, :stop_import
    put "/safety/alerts/:source/:source_id", AlertsController, :create
  end

  pipeline :graphql do
    plug :accepts, ["json"]
    plug :check_auth
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

  forward "/healthcheck", HealthCheckup

  # Provide a GraphiQL interface in dev mode
  if Mix.env() == :dev do
    forward "/graphiql", Absinthe.Plug.GraphiQL,
      schema: WockyAPI.Schema,
      socket: WockyAPI.Channels.UserSocket,
      interface: :playground
  end
end
