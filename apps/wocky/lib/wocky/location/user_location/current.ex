defmodule Wocky.Location.UserLocation.Current do
  @moduledoc false

  import Ecto.Query

  alias Timex.Duration
  alias Wocky.Account.User
  alias Wocky.Friends.Friend
  alias Wocky.Location.UserLocation
  alias Wocky.Location.UserLocation.LocationChangedEvent
  alias Wocky.Repo

  # Expire current location after 2 days
  @expire_secs Duration.from_days(2)
               |> Duration.to_seconds(truncate: true)
               |> to_string()

  @spec set(User.tid(), UserLocation.t()) :: :ok
  def set(user, loc) do
    {:ok, _} =
      Redix.command(Redix, [
        "SET",
        key(User.id(user)),
        value(loc),
        "EX",
        @expire_secs
      ])

    _ =
      %LocationChangedEvent{location: loc, user: User.hydrate(user)}
      |> Dawdle.signal(direct: true)

    :ok
  end

  @spec get(User.tid()) :: UserLocation.t() | nil
  def get(user) do
    case Redix.command(Redix, ["GET", key(user)]) do
      {:ok, nil} ->
        nil

      {:ok, bin} ->
        :erlang.binary_to_term(bin)
    end
  end

  @spec delete(User.tid()) :: :ok
  def delete(user) do
    {:ok, _} = Redix.command(Redix, ["DEL", key(user)])
    :ok
  end

  @spec delete_when_not_shared([User.id()]) :: non_neg_integer()
  def delete_when_not_shared(user_ids) do
    have_shares =
      Friend
      |> select([i], i.user_id)
      |> where([i], i.user_id in ^user_ids)
      |> where([i], i.share_type != "disabled")
      |> distinct(true)
      |> Repo.all()

    (user_ids -- have_shares)
    |> Enum.map(&delete/1)
    |> length()
  end

  defp key(user), do: "current_loc:" <> User.id(user)

  defp value(%UserLocation{} = loc), do: :erlang.term_to_binary(loc)
end
