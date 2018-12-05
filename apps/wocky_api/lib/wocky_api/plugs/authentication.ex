defmodule WockyAPI.Plugs.Authentication do
  @moduledoc "Plugs for performing token authentication"

  import Plug.Conn

  alias Wocky.Account

  def check_location_auth(conn, _opts \\ []) do
    authenticate(conn, &Account.authenticate_for_location/1)
  end

  def check_auth(conn, _opts \\ []) do
    authenticate(conn, &Account.authenticate/1)
  end

  defp authenticate(conn, auth) do
    header = conn |> get_req_header("authentication") |> List.first()

    case parse_jwt_header(header) do
      {:ok, nil} -> conn
      {:ok, token} -> do_authenticate(conn, token, auth)
      {:error, error} -> fail_authentication(conn, error)
    end
  end

  defp parse_jwt_header(nil), do: {:ok, nil}

  defp parse_jwt_header("Bearer " <> token), do: {:ok, String.trim(token)}

  defp parse_jwt_header(_header), do: {:error, :bad_request}

  defp do_authenticate(conn, token, auth) do
    case auth.(token) do
      {:ok, user} ->
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
