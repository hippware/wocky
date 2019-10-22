defmodule Wocky.Location.UserLocation.Current do
  @moduledoc false

  import Ecto.Query

  alias Timex.Duration
  alias Wocky.Account.User
  alias Wocky.Location.UserLocation
  alias Wocky.Location.UserLocation.LocationChangedEvent
  alias Wocky.Repo
  alias Wocky.Roster.Item

  # Expire current location after 2 days
  @expire_secs Duration.from_days(2)
               |> Duration.to_seconds(truncate: true)
               |> to_string()

  @spec set(User.t(), UserLocation.t()) :: :ok
  def set(user, loc) do
    {:ok, _} =
      Redix.command(Redix, [
        "SET",
        key(user.id),
        value(loc),
        "EX",
        @expire_secs
      ])

    _ =
      %LocationChangedEvent{location: loc, user: user}
      |> Dawdle.signal(direct: true)

    :ok
  end

  @spec get(User.t()) :: UserLocation.t() | nil
  def get(user) do
    case Redix.command(Redix, ["GET", key(user.id)]) do
      {:ok, nil} ->
        nil

      {:ok, bin} ->
        :erlang.binary_to_term(bin)
    end
  end

  @spec delete(User.id() | User.t()) :: :ok
  def delete(%User{id: id}), do: delete(id)

  def delete(user_id) do
    {:ok, _} = Redix.command(Redix, ["DEL", key(user_id)])
    :ok
  end

  @spec delete_when_not_shared([User.id()]) :: non_neg_integer()
  def delete_when_not_shared(user_ids) do
    have_shares =
      Item
      |> select([i], i.user_id)
      |> where([i], i.user_id in ^user_ids)
      |> where([i], i.share_type != "disabled")
      |> distinct(true)
      |> Repo.all()

    (user_ids -- have_shares)
    |> Enum.map(&delete/1)
    |> length()
  end

  defp key(user_id), do: "current_loc:" <> user_id

  defp value(%UserLocation{} = loc), do: :erlang.term_to_binary(loc)
end
