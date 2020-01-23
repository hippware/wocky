defmodule WockyAPI.Controllers.FallbackController do
  @moduledoc """
  Translates controller action results into valid `Plug.Conn` responses.

  See `Phoenix.Controller.action_fallback/1` for more details.
  """
  use WockyAPI, :controller

  alias WockyAPI.Views.ErrorView

  @impl true
  def call(conn, {:error, :not_found}) do
    conn
    |> put_status(:not_found)
    |> put_view(ErrorView)
    |> render(:"404")
  end

  def call(conn, {:error, :missing_keys}) do
    conn
    |> put_status(:bad_request)
    |> put_view(ErrorView)
    |> render(:"400", %{message: "JSON payload missing required keys."})
  end
end
