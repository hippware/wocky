defmodule Wocky.User.LocationShare.Cache do
  @moduledoc """
  A write-through cache for user location shares
  """

  import Ecto.Query

  alias Wocky.Repo
  alias Wocky.User
  alias Wocky.User.LocationShare

  @spec get(User.id()) :: [User.id()]
  def get(user_id) do
    {:ok, members} = Redix.command(Redix, ["GET", key(user_id)])
    now = DateTime.utc_now()

    members
    |> decode()
    |> Enum.filter(fn {_, expiry} -> DateTime.compare(expiry, now) == :gt end)
    |> Enum.map(&elem(&1, 0))
  end

  @spec refresh(User.id()) :: :ok
  def refresh(user_id) do
    values =
      LocationShare
      |> where([ls], ls.user_id == ^user_id)
      |> where([ls], ls.expires_at > ^DateTime.utc_now())
      |> select([ls], {ls.shared_with_id, ls.expires_at})
      |> Repo.all()
      |> encode()

    {:ok, _} = Redix.command(Redix, ["SET", key(user_id), values])

    :ok
  end

  defp key(user_id), do: "location_shares:" <> user_id

  defp encode(values), do: :erlang.term_to_binary(values)

  defp decode(nil), do: []
  defp decode(values), do: :erlang.binary_to_term(values)
end
