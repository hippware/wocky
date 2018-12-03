defmodule WockyAPI.Plugs.Authentication do
  @moduledoc "Plugs for performing token authentication"

  import Plug.Conn

  alias Wocky.Account

  def check_auth_headers(conn, _opts \\ []) do
    auth = conn |> get_req_header("authentication") |> List.first()
    authenticate(conn, auth)
  end

  # Anonymous user - limited access
  defp authenticate(conn, nil), do: conn

  # Client JWT authentication
  defp authenticate(conn, auth) do
    case parse_jwt_header(auth) do
      {:ok, token} -> do_authenticate(conn, token)
      {:error, _} -> fail_authentication(conn, :bad_request)
    end
  end

  defp parse_jwt_header("Bearer " <> token), do: {:ok, String.trim(token)}

  defp parse_jwt_header(_header), do: {:error, :bad_jwt_header}

  defp do_authenticate(conn, creds) do
    case Account.authenticate(:jwt, creds) do
      {:ok, {user, _}} ->
        assign(conn, :current_user, user)

      {:error, _} ->
        fail_authentication(conn)
    end
  end

  def ensure_authenticated(conn, _opts \\ []) do
    if Map.get(conn.assigns, :current_user) do
      conn
    else
      fail_authentication(conn)
    end
  end

  defp fail_authentication(conn, reason \\ :unauthorized),
    do: conn |> send_resp(reason, "") |> halt()

  def ensure_owner(conn, _opts \\ []) do
    path_user = conn.path_params["user_id"]
    user = Map.get(conn.assigns, :current_user)

    if is_nil(path_user) || (!is_nil(user) && user.id == path_user) do
      conn
    else
      fail_authentication(conn, :forbidden)
    end
  end
end
