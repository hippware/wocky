defmodule WockyAPI.AuthenticationTest do
  use WockyAPI.ConnCase

  import WockyAPI.Authentication

  alias Faker.Lorem
  alias Wocky.Account
  alias Wocky.Repo.Factory
  alias Wocky.Repo.ID
  alias Wocky.User

  def put_auth_headers(conn, user, token) do
    conn
    |> put_req_header("x-auth-user", user)
    |> put_req_header("x-auth-token", token)
  end

  describe ":authenticate plug" do
    setup do
      user = Factory.insert(:user)
      resource = Faker.Code.issn()
      {:ok, {token, _}} = Account.assign_token(user.id, resource)

      {:ok, token: token, user_id: user.id}
    end

    test "valid user and token", context do
      conn =
        context.conn
        |> put_auth_headers(context.user_id, context.token)
        |> authenticate

      assert conn.assigns.current_user
    end

    test "valid user, invalid token", context do
      conn =
        context.conn
        |> put_auth_headers(context.user_id, "foo")
        |> authenticate

      assert conn.status == 401
      assert conn.halted
    end

    test "valid user, no token", context do
      conn =
        context.conn
        |> put_req_header("x-auth-user", context.user_id)
        |> authenticate

      assert conn.status == 401
      assert conn.halted
    end

    test "invalid user", context do
      conn =
        context.conn
        |> put_auth_headers("foo", context.token)
        |> authenticate

      assert conn.status == 401
      assert conn.halted
    end

    test "no user", context do
      conn =
        context.conn
        |> put_req_header("x-auth-token", context.token)
        |> authenticate

      assert conn.status == 401
      assert conn.halted
    end
  end

  defmodule Param do
    use Plug.Router
    use Plug.ErrorHandler

    plug :match
    plug :dispatch

    match "/:user_id" do
      resp(conn, 200, conn.path_params["user_id"])
    end

    match "/none" do
      resp(conn, 200, "")
    end
  end

  defp setup_conn(url \\ "/none") do
    :post |> build_conn(url) |> Param.call([])
  end

  describe ":check_owner_access plug" do
    test "no user ID in URL, no current user", context do
      conn =
        context.conn
        |> check_owner_access

      refute conn.halted
    end

    test "no user ID in URL, current user", context do
      conn =
        context.conn
        |> assign(:current_user, %User{})
        |> check_owner_access

      refute conn.halted
    end

    test "user ID in URL, no current user", _context do
      conn =
        setup_conn()
        |> check_owner_access

      assert conn.status == 403
      assert conn.halted
    end

    test "current user matches user ID in URL", _context do
      user = Factory.build(:user)

      conn =
        "/#{user.id}"
        |> setup_conn()
        |> assign(:current_user, user)
        |> check_owner_access

      refute conn.halted
    end

    test "current user does not match user ID in URL", _context do
      user = Factory.build(:user)

      conn =
        "/#{ID.new()}"
        |> setup_conn()
        |> assign(:current_user, user)
        |> check_owner_access

      assert conn.status == 403
      assert conn.halted
    end
  end

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
