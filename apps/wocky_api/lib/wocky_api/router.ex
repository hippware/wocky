defmodule WockyAPI.Router do
  use WockyAPI, :router

  pipeline :api do
    plug :accepts, ["json"]
  end

  scope "/api/v1", WockyAPI do
    pipe_through :api
  end
end
