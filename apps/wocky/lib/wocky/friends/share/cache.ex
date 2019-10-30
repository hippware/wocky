defmodule Wocky.Friends.Share.Cache do
  @moduledoc """
  A write-through cache for user location shares
  """

  import Ecto.Query

  alias Timex.Duration
  alias Wocky.Account.User
  alias Wocky.Friends.Friend
  alias Wocky.Repo

  # Let Redis expire an untouched cache after two weeks - cleans up any deleted
  # users but shouldn't add any significant refresh overhead.
  @expire_secs Duration.from_weeks(2)
               |> Duration.to_seconds(truncate: true)
               |> to_string()

  @spec get(User.id()) :: [User.id()]
  def get(user_id) do
    {:ok, members} = Redix.command(Redix, ["GET", key(user_id)])

    case members do
      nil ->
        refresh(user_id)

      _ ->
        members
        |> decode()
        |> Enum.map(fn
          {id, _} -> id
          id -> id
        end)
    end
  end

  @spec refresh(User.id()) :: [User.id()]
  def refresh(user_id) do
    values =
      Friend
      |> where([i], i.user_id == ^user_id)
      |> where([i], i.share_type != "disabled")
      |> select([i], i.contact_id)
      |> Repo.all()

    put(user_id, values)

    values
  end

  # Public for testing purposes
  @doc false
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
  def key(user_id), do: "location_shares:" <> user_id

  defp encode(values), do: :erlang.term_to_binary(values)

  defp decode(values), do: :erlang.binary_to_term(values)
end
