defmodule WockyAPI.Controllers.GeometriesController do
  use Elixometer
  use WockyAPI, :controller

  alias Plug.Conn
  alias Wocky.Alerts

  action_fallback WockyAPI.Controllers.FallbackController

  @spec create(Conn.t(), Keyword.t()) :: Conn.t() | {:error, any()}
  def create(conn, params) do
    geometry = %{
      source: conn.path_params["source"],
      source_id: conn.path_params["source_id"],
      geometry: params["geometry"],
      data: params["data"] || %{}
    }

    with {:ok, _} <- Alerts.insert_geometry(geometry) do
      send_resp(conn, :created, "")
    end
  end
end
