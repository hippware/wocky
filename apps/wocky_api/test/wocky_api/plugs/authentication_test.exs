defmodule WockyAPI.Plugs.AuthenticationTest do
  use WockyAPI.ConnCase

  import WockyAPI.Plugs.Authentication

  alias Wocky.Repo.Factory
  alias Wocky.Repo.ID
  alias Wocky.User

  def put_jwt_header(conn, jwt, prefix \\ "Bearer ") do
    put_req_header(conn, "authentication", prefix <> jwt)
  end

  describe ":check_auth_headers plug with JWT auth" do
    setup do
      user = Factory.insert(:user)
      jwt = Factory.get_test_token(user)

      {:ok, jwt: jwt, user: user, user_id: user.id}
    end

    test "valid client JWT header", context do
      conn =
        context.conn
        |> put_jwt_header(context.jwt)
        |> check_auth_headers

      assert conn.assigns.current_user
    end

    test "valid server JWT header", context do
      jwt = Factory.get_test_location_token(context.user)

      conn =
        context.conn
        |> put_jwt_header(jwt)
        |> check_auth_headers

      assert conn.assigns.current_user
    end

    test "invalid JWT header format", context do
      conn =
        context.conn
        |> put_jwt_header(context.jwt, "")
        |> check_auth_headers

      assert conn.status == 400
      assert conn.halted
    end

    test "invalid JWT in header", context do
      conn =
        context.conn
        |> put_jwt_header("foo")
        |> check_auth_headers

      assert conn.status == 401
      assert conn.halted
    end
  end

  describe ":ensure_authenticated plug" do
    test "when current_user is assigned", context do
      conn =
        context.conn
        |> assign(:current_user, :foo)
        |> ensure_authenticated

      refute conn.halted
    end

    test "when current_user is not assigned", context do
      conn =
        context.conn
        |> ensure_authenticated

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

  describe ":ensure_owner plug" do
    test "no user ID in URL, no current user", context do
      conn =
        context.conn
        |> ensure_owner

      refute conn.halted
    end

    test "no user ID in URL, current user", context do
      conn =
        context.conn
        |> assign(:current_user, %User{})
        |> ensure_owner

      refute conn.halted
    end

    test "user ID in URL, no current user", _context do
      conn =
        setup_conn()
        |> ensure_owner

      assert conn.status == 403
      assert conn.halted
    end

    test "current user matches user ID in URL", _context do
      user = Factory.build(:user)

      conn =
        "/#{user.id}"
        |> setup_conn()
        |> assign(:current_user, user)
        |> ensure_owner

      refute conn.halted
    end

    test "current user does not match user ID in URL", _context do
      user = Factory.build(:user)

      conn =
        "/#{ID.new()}"
        |> setup_conn()
        |> assign(:current_user, user)
        |> ensure_owner

      assert conn.status == 403
      assert conn.halted
    end
  end
end
