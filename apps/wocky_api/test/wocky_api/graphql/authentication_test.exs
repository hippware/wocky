defmodule WockyAPI.GraphQL.AuthenticationTest do
  use WockyAPI.ConnCase

  import WockyAPI.Authentication

  alias Wocky.Account
  alias Wocky.Repo.Factory

  describe "GraphQL in-band authentication" do
    setup do
      user = Factory.insert(:user)
      {:ok, {token, _}} = Account.assign_token(user.id, "abc")
      {:ok, user: user, token: token, conn: build_conn()}
    end

    @query """
    mutation ($input: AuthenticateInput!) {
      authenticate (input: $input) {
        user {
          id
        }
      }
    }
    """
    test "successful authentication", %{user: user, token: token, conn: conn} do
      assert post_conn(conn, @query,
                       %{input: %{user: user.id, token: token}}, 200) ==
        %{
          "data" => %{
            "authenticate" => %{
              "user" => %{
                "id" => user.id
              }
            }
          }
        }
    end

    test "unsuccessful authentication", %{user: user, conn: conn} do
      assert %{
        "data" => %{"authenticate" => nil},
        "errors" => [_]
      } =
      post_conn(conn, @query,
                %{input: %{user: user.id, token: Lorem.word()}}, 200)
    end
  end
end
