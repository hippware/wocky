defmodule WockyAPI.Authentication do
  @moduledoc "Plugs for performing token authentication"

  import Plug.Conn

  alias Wocky.Repo
  alias Wocky.Token
  alias Wocky.User

  def authenticate(conn, _opts \\ []) do
    user_id = conn |> get_req_header("x-auth-user") |> List.first()
    token = conn |> get_req_header("x-auth-token") |> List.first()

    if Token.valid?(user_id, token) do
      # This can't return nil since the token check passed.
      user = Repo.get!(User, user_id)

      conn
      |> assign(:current_user, user)
      |> put_private(:absinthe, %{context: %{current_user: user}})
    else
      conn |> send_resp(:unauthorized, "") |> halt()
    end
  end

  def check_owner_access(conn, _opts \\ []) do
    path_user = conn.path_params["user_id"]
    user = Map.get(conn.assigns, :current_user)

    if is_nil(path_user) || (!is_nil(user) && user.id == path_user) do
      conn
    else
      conn |> send_resp(:forbidden, "") |> halt()
    end
  end
end
