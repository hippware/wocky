defmodule Wocky.Location.Share.Cache do
  @moduledoc """
  A write-through cache for user location shares
  """

  import Ecto.Query

  alias Wocky.Account.User
  alias Wocky.Location.Share
  alias Wocky.Repo

  @spec get(User.id()) :: [User.id()]
  def get(user_id) do
    {:ok, members} = Redix.command(Redix, ["GET", key(user_id)])

    case members do
      nil ->
        refresh(user_id)

      _ ->
        now = DateTime.utc_now()

        members
        |> decode()
        |> Enum.filter(fn {_, expiry} ->
          DateTime.compare(expiry, now) == :gt
        end)
        |> Enum.map(&elem(&1, 0))
    end
  end

  @spec refresh(User.id()) :: [User.id()]
  def refresh(user_id) do
    values =
      Share
      |> where([ls], ls.user_id == ^user_id)
      |> where([ls], ls.expires_at > ^DateTime.utc_now())
      |> select([ls], {ls.shared_with_id, ls.expires_at})
      |> Repo.all()

    put(user_id, values)

    Enum.map(values, fn {id, _} -> id end)
  end

  # Public for testing purposes
  @doc false
  def put(user_id, values) do
    {:ok, _} = Redix.command(Redix, ["SET", key(user_id), encode(values)])
    :ok
  end

  @doc false
  def key(user_id), do: "location_shares:" <> user_id

  defp encode(values), do: :erlang.term_to_binary(values)

  defp decode(values), do: :erlang.binary_to_term(values)
end
