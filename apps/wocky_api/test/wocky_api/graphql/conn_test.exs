defmodule WockyAPI.GraphQL.ConnTest do
  @moduledoc """
  GraphQL tests specifically requiring running through the full
  Phoenix pipeline
  """

  use WockyAPI.ConnCase, async: true

  alias Wocky.Account
  alias Wocky.Repo.Factory

  setup do
    user = Factory.insert(:user)
    {:ok, {token, _}} = Account.assign_token(user.id, "abc")

    conn =
      build_conn()
      |> put_req_header("x-auth-user", user.id)
      |> put_req_header("x-auth-token", token)

    {:ok, conn: conn}
  end

  describe "complexity" do
    @query """
    query {
      currentUser {
        bots (first: 50, relationship: OWNED) {
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
      result = post_conn(conn, @query, 400)

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
