defmodule WockyAPI.Router do
  use WockyAPI, :router
  use Honeybadger.Plug

  import WockyAPI.Authentication


  pipeline :api do
    plug :accepts, ["json"]
    plug :authenticate
    plug :check_owner_access
  end

  scope "/api/v1", WockyAPI do
    pipe_through :api

    resources "/users/:user_id/locations", LocationController, only: [:create]
  end
end
