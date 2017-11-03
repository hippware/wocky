defmodule WockyAPI.Router do
  use WockyAPI, :router

  import WockyAPI.Authentication


  pipeline :api do
    plug :accepts, ["json"]
    plug :authenticate
    plug :check_owner_access
  end

  scope "/api/v1", WockyAPI do
    pipe_through :api
  end
end
