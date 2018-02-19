defmodule WockyAPI.Router do
  use WockyAPI, :router
  use Honeybadger.Plug

  import WockyAPI.Authentication

  pipeline :rest_api do
    plug :accepts, ["json"]
    plug :authenticate
    plug :check_owner_access
  end

  scope "/api/v1", WockyAPI do
    pipe_through :rest_api

    resources "/users/:user_id/locations", LocationController, only: [:create]
  end

  pipeline :graphql do
    plug :accepts, ["json"]
    plug :authenticate
  end

  scope "/" do
    pipe_through :graphql

    forward "/graphiql", Absinthe.Plug.GraphiQL,
      schema: WockyAPI.Schema,
      interface: :simple,
      context: %{pubsub: WockyAPI.Endpoint}
  end
end
