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

    Factory.insert_list(50, :bot, user: user)
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
      post_conn(conn, @query, 200)
      |> (fn a -> a["data"]["currentUser"]["bots"]["edges"] end).()
      |> length()
    end
  end

  defp post_conn(conn, query, variables \\ %{}, code) do
    conn
    |> post("/graphql", query: query, variables: variables)
    |> json_response(code)
  end
end
