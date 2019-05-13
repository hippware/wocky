defmodule Wocky.User.CurrentLocation do
  @moduledoc false

  alias Wocky.{CallbackManager, User}
  alias Wocky.User.Location

  @type callback() :: (User.t(), Location.t() -> :ok)

  @spec register_callback(callback()) :: :ok
  def register_callback(cb), do: CallbackManager.add(__MODULE__, cb)

  @spec set(User.t(), Location.t()) :: :ok
  def set(user, loc) do
    {:ok, _} = Redix.command(Redix, ["SET", key(user.id), value(loc)])

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

  defp key(user_id), do: "current_loc:" <> user_id

  defp value(%Location{} = loc), do: :erlang.term_to_binary(loc)
end
