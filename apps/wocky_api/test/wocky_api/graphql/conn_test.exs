defmodule WockyAPI.GraphQL.ConnTest do
  @moduledoc """
  GraphQL tests specifically requiring running through the full
  Phoenix pipeline
  """

  use WockyAPI.ConnCase, async: true

  alias Wocky.Repo.Factory

  setup do
    user = Factory.insert(:user)
    token = Factory.get_test_token(user)
    conn = put_req_header(build_conn(), "authentication", "Bearer #{token}")

    {:ok, conn: conn}
  end

  describe "complexity" do
    @query """
    query {
      currentUser {
        bots (first: 5000, relationship: OWNED) {
          edges {
            node {
              id
              title
              description
              image {
                tros_url
                full_url
              }
            }
          }
        }
      }
    }
    """
    test "Large sets of bots should exceed complexity limit", %{conn: conn} do
      result = post_conn(conn, @query, 200)

      assert error_msg(result, 2) =~ "Operation is too complex"
    end
  end

  defp post_conn(conn, query, variables \\ %{}, code) do
    conn
    |> post("/graphql", query: query, variables: variables)
    |> json_response(code)
  end

  defp error_msg(result, idx) do
    result
    |> Map.get("errors", [])
    |> Enum.at(idx, %{})
    |> Map.get("message", "")
  end
end
