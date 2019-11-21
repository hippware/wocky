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

  @spec get(User.tid()) :: [CachedFriend.t()]
  def get(user) do
    {:ok, cache} = Redix.command(Redix, ["GET", key(user)])

    case cache do
      nil ->
        refresh(user)

      _ ->
        case decode(cache) do
          {@cache_version, members} ->
            Enum.map(members, &cache_to_struct/1)

          # Older cache format
          x when is_list(x) ->
            refresh(user)
        end
    end
  end

  @spec refresh(User.tid()) :: [CachedFriend.t()]
  def refresh(user) do
    values =
      Friend
      |> where([i], i.user_id == ^User.id(user))
      |> where([i], i.share_type != "disabled")
      |> Repo.all()

    put(user, {@cache_version, Enum.map(values, &struct_to_cache/1)})

    Enum.map(values, &Friend.to_cached/1)
  end

  # Public for testing purposes
  @doc false
  @spec put(User.tid(), any()) :: :ok
  def put(user, values) do
    {:ok, _} =
      Redix.command(Redix, [
        "SET",
        key(user),
        encode(values),
        "EX",
        @expire_secs
      ])

    :ok
  end

  @doc false
  @spec key(User.tid()) :: String.t()
  def key(user), do: "location_shares:" <> User.id(user)

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
