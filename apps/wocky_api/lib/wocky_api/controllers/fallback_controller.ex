defmodule WockyAPI.FallbackController do
  @moduledoc """
  Translates controller action results into valid `Plug.Conn` responses.

  See `Phoenix.Controller.action_fallback/1` for more details.
  """
  use WockyAPI, :controller

  def call(conn, {:error, :not_found}) do
    conn
    |> put_status(:not_found)
    |> put_view(WockyAPI.ErrorView)
    |> render("404.json", [])
  end

  def call(conn, {:error, :missing_keys}) do
    conn
    |> put_status(:bad_request)
    |> put_view(WockyAPI.ErrorView)
    |> render("error.json", message: "JSON missing required keys.")
  end
end
