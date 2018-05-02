defmodule WockyAPI.Plugs.AbsintheConnData do
  @moduledoc "Plug for saving conn info to Absinthe data"

  import Plug.Conn

  def load_graphql_context(conn, _opts \\ []) do
    context = %{peer: peer(conn), host: host()}

    user = Map.get(conn.assigns, :current_user)

    context =
      if user do
        Map.put(context, :current_user, user)
      else
        context
      end

    put_private(conn, :absinthe, %{context: context})
  end

  def save_conn_data(conn, _opts \\ []) do
    conn
    |> put_private(:absinthe, %{context: %{peer: peer(conn)}})
    |> put_private(:absinthe, %{context: %{host: host()}})
  end

  defp peer(conn) do
    {addr, port} = conn.peer
    to_string(:inet.ntoa(addr)) <> ":" <> to_string(port)
  end

  defp host do
    {:ok, host} = :inet.gethostname()
    to_string(host)
  end
end
