defmodule WockyAPI.ConnHelper do
  @moduledoc """
  Helper functions for Wocky API connection tests
  """

  use Phoenix.ConnTest

  @endpoint WockyAPI.Endpoint

  def post_conn(conn, query, variables \\ %{}, code) do
    conn
    |> post("/graphql", query: query, variables: variables)
    |> json_response(code)
  end
end
