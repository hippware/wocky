defmodule Wocky.User.Token do
  @moduledoc "Handles generation and validation of use authentication tokens."

  alias Wocky.Repo
  alias Wocky.Repo.Object
  alias Wocky.User

  @type t :: binary
  @type expiry :: pos_integer

  @token_bytes 32
  @token_marker "$T$"
  @token_expire 1209600 # 2 weeks

  @doc "Generates a token"
  @spec new :: t
  def new do
    string =
      @token_bytes
      |> :crypto.strong_rand_bytes
      |> Base.encode64

    @token_marker <> string
  end

  @doc "Generates a token and assigns it to the specified user and resource."
  @spec assign(User.id, User.server, User.resource) :: {:ok, {t, expiry}}
  def assign(id, server, resource) do
    token = new()
    expiry = now() + @token_expire

    token_map = %{
      user_id: id,
      server: server,
      resource: resource,
      token: token,
      created_at: now(),
      expires_at: expiry
    }

    :ok =
      Map.new
      |> Map.put(resource, token_map)
      |> Repo.update("tokens", server, id)

    {:ok, {token, expiry}}
  end

  defp now, do: DateTime.utc_now |> DateTime.to_unix

  @doc "Return the token assigned to the specified user and resource."
  @spec get_token(User.id, User.server, User.resource) :: t | nil
  def get_token(id, server, resource) do
    id
    |> get_token_map(server)
    |> Map.get(resource |> String.to_atom)
    |> token_from_map()
  end

  defp get_token_map(id, server) do
    "tokens" |> Repo.find(server, id) |> Object.to_map
  end

  defp token_from_map(nil), do: nil
  defp token_from_map(map), do: Map.get(map, :token)

  @doc """
  Returns all tokens currently assigned to resources belonging to the
  specified user.
  """
  @spec get_tokens(User.id, User.server) :: [t]
  def get_tokens(id, server) do
    id
    |> get_token_map(server)
    |> Enum.map(fn {_, v} -> Map.get(v, :token) end)
  end

  @doc """
  Returns `true' if a token is valid for the supplied
  user or `false' otherwise.
  """
  @spec valid?(User.id, User.server, t) :: boolean
  def valid?(id, server, token) do
    id
    |> get_token_map(server)
    |> Enum.reduce_while(false, &check_token(token, &1, &2))
  end

  defp check_token(token, {_, %{token: token} = v}, _),
    do: {:halt, !expired?(v[:expires_at])}
  defp check_token(_, _, _), do: {:cont, false}

  defp expired?(expiry), do: String.to_integer(expiry) < now()

  @doc """
  Releases any token currently assigned to the specified user and resource.
  """
  @spec release(User.id, User.server, User.resource) :: :ok
  def release(id, server, resource) do
    :ok =
      "tokens"
      |> Repo.find(server, id)
      |> Riak.CRDT.Map.delete({resource, :map})
      |> Repo.update("tokens", server, id)
  end
end
