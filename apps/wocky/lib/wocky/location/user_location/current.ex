defmodule Wocky.Location.UserLocation.Current do
  @moduledoc false

  alias Timex.Duration
  alias Wocky.Account.User
  alias Wocky.Contacts
  alias Wocky.Errors
  alias Wocky.Location.UserLocation
  alias Wocky.Location.UserLocation.LocationChangedEvent

  # Expire current location after 2 days
  @expire_secs Duration.from_days(2)
               |> Duration.to_seconds(truncate: true)
               |> to_string()

  @spec set(User.tid(), UserLocation.t()) :: :ok | {:error, any()}
  def set(user, loc) do
    with {:ok, _} <- do_set(user, loc) do
      user = User.hydrate(user)

      Errors.log_on_failure(
        "Signaling location changed for user #{user.id}",
        fn ->
          %LocationChangedEvent{location: loc, user: user}
          |> Dawdle.signal(direct: true)
        end
      )
    end
  end

  defp do_set(user, loc) do
    Redix.command(Redix, [
      "SET",
      key(User.id(user)),
      value(loc),
      "EX",
      @expire_secs
    ])
  end

  @spec get(User.tid()) :: {:ok, UserLocation.t() | nil} | {:error, any()}
  def get(user) do
    case Redix.command(Redix, ["GET", key(user)]) do
      {:ok, nil} ->
        {:ok, nil}

      {:ok, bin} ->
        {:ok, :erlang.binary_to_term(bin)}

      {:error, _} = error ->
        error
    end
  end

  @spec delete(User.tid()) :: :ok | {:error, any()}
  def delete(user) do
    with {:ok, _} <- Redix.command(Redix, ["DEL", key(user)]) do
      :ok
    end
  end

  @spec delete_when_not_shared([User.id()]) :: non_neg_integer()
  def delete_when_not_shared(user_ids) do
    have_shares = Contacts.have_shares(user_ids)

    (user_ids -- have_shares)
    |> Enum.map(&do_quiet_delete/1)
    |> length()
  end

  defp do_quiet_delete(user_id) do
    Errors.log_on_failure(
      "Deleting cached current location for user #{user_id}",
      fn -> delete(user_id) end
    )
  end

  defp key(user), do: "current_loc:" <> User.id(user)

  defp value(%UserLocation{} = loc), do: :erlang.term_to_binary(loc)
end
