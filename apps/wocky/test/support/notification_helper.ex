defmodule Wocky.NotificationHelper do
  @moduledoc "Helper functions for dealing with in-band notifications"
  alias Wocky.Account.User
  alias Wocky.Repo

  import Ecto
  import Ecto.Query

  @retry_interval 200

  @spec clear_expected_notifications(User.t(), non_neg_integer()) :: :ok
  def clear_expected_notifications(user, count) do
    wait_for_notifications(user, count, 2000)
  end

  defp wait_for_notifications(user, count, timeout) when timeout <= 0 do
    throw(
      "Timeout waiting for user #{inspect(user.id)} to have #{count} notifications"
    )
  end

  defp wait_for_notifications(user, count, timeout) do
    query = from(n in assoc(user, :notifications))
    db_count = query |> select([n], count(1)) |> Repo.one()

    cond do
      db_count < count ->
        Process.sleep(@retry_interval)
        wait_for_notifications(user, count, timeout < @retry_interval)

      db_count > count ->
        throw("Too many notifications for user #{inspect(user.id)}")

      true ->
        Repo.delete_all(query)
    end
  end
end
