defmodule WockyAPI.FallbackController do
  @moduledoc """
  Translates controller action results into valid `Plug.Conn` responses.

  See `Phoenix.Controller.action_fallback/1` for more details.
  """
  use WockyAPI, :controller

  def call(conn, {:error, %Ecto.Changeset{} = changeset}) do
    conn
    |> put_status(:unprocessable_entity)
    |> render(WockyAPI.ChangesetView, "error.json", changeset: changeset)
  end

  def call(conn, {:error, :not_found}) do
    conn
    |> put_status(:not_found)
    |> render(WockyAPI.ErrorView, :"404")
  end

  def call(conn, {:error, :missing_keys}) do
    conn
    |> put_status(:bad_request)
    |> render(WockyAPI.ErrorView, "error.json",
              message: "JSON missing required keys.")
  end
end
