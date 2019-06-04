defmodule Wocky.User.CurrentLocation do
  @moduledoc false

  import Ecto.Query

  alias Wocky.{CallbackManager, Repo, User}
  alias Wocky.User.{Location, LocationShare}

  @type callback() :: (User.t(), Location.t() -> :ok)

  # 2 days
  @expire_secs 60 * 60 * 24 * 2

  @spec register_callback(callback()) :: :ok
  def register_callback(cb), do: CallbackManager.add(__MODULE__, cb)

  @spec set(User.t(), Location.t()) :: :ok
  def set(user, loc) do
    {:ok, _} =
      Redix.command(Redix, [
        "SET",
        key(user.id),
        value(loc),
        "EX",
        to_string(@expire_secs)
      ])

    __MODULE__
    |> CallbackManager.get()
    |> Enum.each(& &1.(user, loc))

    :ok
  end

  @spec get(User.t()) :: Location.t() | nil
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
      LocationShare
      |> select([ls], ls.user_id)
      |> where([ls], ls.user_id in ^user_ids)
      |> distinct(true)
      |> Repo.all()

    (user_ids -- have_shares)
    |> Enum.map(&delete/1)
    |> length()
  end

  defp key(user_id), do: "current_loc:" <> user_id

  defp value(%Location{} = loc), do: :erlang.term_to_binary(loc)
end
