defmodule Wocky.Contacts.Share.Cache do
  @moduledoc """
  A write-through cache for user location shares
  """

  import Ecto.Query

  alias Timex.Duration
  alias Wocky.Account.User
  alias Wocky.Contacts.Relationship
  alias Wocky.Contacts.Share.CachedRelationship
  alias Wocky.Repo

  @cache_version 1

  # Let Redis expire an untouched cache after two weeks - cleans up any deleted
  # users but shouldn't add any significant refresh overhead.
  @expire_secs Duration.from_weeks(2)
               |> Duration.to_seconds(truncate: true)
               |> to_string()

  @spec get(User.tid()) :: [CachedRelationship.t()]
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

  @spec refresh(User.tid()) :: [CachedRelationship.t()]
  def refresh(user) do
    values =
      Repo.all(
        from r in Relationship,
          where: r.user_id == ^User.id(user) and r.share_type != "disabled"
      )

    put(user, {@cache_version, Enum.map(values, &struct_to_cache/1)})

    Enum.map(values, &CachedRelationship.new/1)
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

  defp struct_to_cache(%Relationship{} = relationship) do
    CachedRelationship.fields()
    |> Enum.map(&Map.get(relationship, &1))
  end

  defp cache_to_struct(members) do
    keyword = List.zip([CachedRelationship.fields(), members])
    struct(CachedRelationship, keyword)
  end
end
