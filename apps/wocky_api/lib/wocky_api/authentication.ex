defmodule WockyAPI.Authentication do
  @moduledoc "Plugs for performing token authentication"

  import Plug.Conn

  alias Wocky.Token

  def authenticate(conn, _opts \\ []) do
    user = conn |> get_req_header("x-auth-user") |> List.first
    token = conn |> get_req_header("x-auth-token") |> List.first

    if Token.valid?(user, token) do
      assign(conn, :current_user, user)
    else
      conn |> send_resp(:unauthorized, "") |> halt()
    end
  end

  def check_owner_access(conn, _opts \\ []) do
    path_user = conn.path_params["user_id"]
    current_user = Map.get(conn.assigns, :current_user)

    if is_nil(path_user) || current_user == path_user do
      conn
    else
      conn |> send_resp(:forbidden, "") |> halt()
    end
  end
end
