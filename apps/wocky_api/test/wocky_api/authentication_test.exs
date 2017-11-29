defmodule WockyAPI.AuthenticationTest do
  use WockyAPI.ConnCase

  import WockyAPI.Authentication

  alias Wocky.Repo.Factory
  alias Wocky.Repo.ID
  alias Wocky.Token

  def put_auth_headers(conn, user, token) do
    conn
    |> put_req_header("x-auth-user", user)
    |> put_req_header("x-auth-token", token)
  end

  describe ":authenticate plug" do
    setup do
      user = Factory.insert(:user)
      resource = Faker.Code.issn
      {:ok, {token, _}} = Token.assign(user.id, resource)

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
        |> assign(:current_user, ID.new)
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
      id = ID.new
      conn =
        "/#{id}"
        |> setup_conn()
        |> assign(:current_user, id)
        |> check_owner_access

      refute conn.halted
    end

    test "current user does not match user ID in URL", _context do
      conn =
        "/#{ID.new}"
        |> setup_conn()
        |> assign(:current_user, ID.new)
        |> check_owner_access

      assert conn.status == 403
      assert conn.halted
    end
  end
end
