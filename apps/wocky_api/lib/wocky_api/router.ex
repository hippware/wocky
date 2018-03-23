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

  scope "/graphql" do
    pipe_through :graphql

    forward "/", Absinthe.Plug,
      schema: WockyAPI.Schema

  end

  # Provide a GraphiQL interface in dev mode
  if Mix.env == :dev do
    forward "/graphiql", Absinthe.Plug.GraphiQL,
      schema: WockyAPI.Schema,
      socket: WockyAPI.UserSocket
  end
end
