defmodule Wocky.Friends.Share.Cache do
  @moduledoc """
  A write-through cache for user location shares
  """

  import Ecto.Query

  alias Timex.Duration
  alias Wocky.Account.User
  alias Wocky.Friends.Friend
  alias Wocky.Friends.Share.CachedFriend
  alias Wocky.Repo

  @cache_version 1

  # Let Redis expire an untouched cache after two weeks - cleans up any deleted
  # users but shouldn't add any significant refresh overhead.
  @expire_secs Duration.from_weeks(2)
               |> Duration.to_seconds(truncate: true)
               |> to_string()

  @spec get(User.id()) :: [CachedFriend.t()]
  def get(user_id) do
    {:ok, cache} = Redix.command(Redix, ["GET", key(user_id)])

    case cache do
      nil ->
        refresh(user_id)

      _ ->
        case decode(cache) do
          {@cache_version, members} ->
            Enum.map(members, &cache_to_struct/1)

          # Older cache format
          x when is_list(x) ->
            refresh(user_id)
        end
    end
  end

  @spec refresh(User.id()) :: [CachedFriend.t()]
  def refresh(user_id) do
    values =
      Friend
      |> where([i], i.user_id == ^user_id)
      |> where([i], i.share_type != "disabled")
      |> Repo.all()

    put(user_id, {@cache_version, Enum.map(values, &struct_to_cache/1)})

    Enum.map(values, &Friend.to_cached/1)
  end

  # Public for testing purposes
  @doc false
  @spec put(User.id(), any()) :: :ok
  def put(user_id, values) do
    {:ok, _} =
      Redix.command(Redix, [
        "SET",
        key(user_id),
        encode(values),
        "EX",
        @expire_secs
      ])

    :ok
  end

  @doc false
  @spec key(User.id()) :: String.t()
  def key(user_id), do: "location_shares:" <> user_id

  defp encode(values), do: :erlang.term_to_binary(values)

  defp decode(values), do: :erlang.binary_to_term(values)

  defp struct_to_cache(%Friend{} = friend) do
    CachedFriend.fields()
    |> Enum.map(&Map.get(friend, &1))
  end

  defp cache_to_struct(members) do
    keyword = List.zip([CachedFriend.fields(), members])
    struct(CachedFriend, keyword)
  end
end
