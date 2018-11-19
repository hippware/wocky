defmodule WockyAPI.Plugs.AuthenticationTest do
  use WockyAPI.ConnCase

  import WockyAPI.Plugs.Authentication

  alias Wocky.Account
  alias Wocky.Account.JWT.Client, as: ClientJWT
  alias Wocky.Account.JWT.Server, as: ServerJWT
  alias Wocky.Repo.Factory
  alias Wocky.Repo.ID
  alias Wocky.User

  def put_token_headers(conn, user, token) do
    conn
    |> put_req_header("x-auth-user", user)
    |> put_req_header("x-auth-token", token)
  end

  def put_jwt_header(conn, jwt, prefix \\ "Bearer ") do
    put_req_header(conn, "authentication", prefix <> jwt)
  end

  describe ":check_auth_headers plug" do
    test "no credentials", context do
      conn = check_auth_headers(context.conn)

      refute conn.assigns[:current_user]
      refute conn.halted
    end
  end

  describe ":check_auth_headers plug with token auth" do
    setup do
      user = Factory.insert(:user)
      resource = Faker.Code.issn()
      {:ok, {token, _}} = Account.assign_token(user.id, resource)

      {:ok, token: token, user_id: user.id}
    end

    test "valid user and token", context do
      conn =
        context.conn
        |> put_token_headers(context.user_id, context.token)
        |> check_auth_headers

      assert conn.assigns.current_user
    end

    test "valid user, invalid token", context do
      conn =
        context.conn
        |> put_token_headers(context.user_id, "foo")
        |> check_auth_headers

      assert conn.status == 401
      assert conn.halted
    end

    test "valid user, no token", context do
      conn =
        context.conn
        |> put_req_header("x-auth-user", context.user_id)
        |> check_auth_headers

      assert conn.status == 401
      assert conn.halted
    end

    test "invalid user", context do
      conn =
        context.conn
        |> put_token_headers("foo", context.token)
        |> check_auth_headers

      assert conn.status == 401
      assert conn.halted
    end

    test "no user", context do
      conn =
        context.conn
        |> put_req_header("x-auth-token", context.token)
        |> check_auth_headers

      assert conn.status == 401
      assert conn.halted
    end
  end

  describe ":check_auth_headers plug with JWT auth" do
    setup do
      user = Factory.insert(:user)
      {:ok, jwt, _} = ClientJWT.encode_and_sign(user)

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
      {:ok, jwt, _} = ServerJWT.encode_and_sign(context.user)

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
