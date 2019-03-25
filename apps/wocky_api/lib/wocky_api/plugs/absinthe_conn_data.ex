defmodule WockyAPI.Plugs.AbsintheConnData do
  @moduledoc "Plug for saving conn info to Absinthe data"

  import Plug.Conn

  def load_graphql_context(conn, _opts \\ []) do
    base_context = %{peer: peer(conn), host: host()}

    user = Map.get(conn.assigns, :current_user)

    context = maybe_set_user(user, base_context)

    put_private(conn, :absinthe, %{context: context})
  end

  defp maybe_set_user(nil, context), do: context

  defp maybe_set_user(user, context), do: Map.put(context, :current_user, user)

  def save_conn_data(conn, _opts \\ []) do
    conn
    |> put_private(:absinthe, %{context: %{peer: peer(conn)}})
    |> put_private(:absinthe, %{context: %{host: host()}})
  end

  defp peer(conn) do
    %{address: addr, port: port} = get_peer_data(conn)
    to_string(:inet.ntoa(addr)) <> ":" <> to_string(port)
  end

  defp host do
    {:ok, host} = :inet.gethostname()
    to_string(host)
  end
end
