defmodule WockyAPI.Presence.Store do
  @moduledoc """
  Redis-based store for user_id -> [presence_pid] mappings
  """

  alias Wocky.User

  @lock_timeout_secs 5

  @doc """
  Add the calling presence-tracking process to the records for a user
  """
  @spec add_self(User.id()) :: :ok
  def add_self(user_id) do
    {:ok, _} = Redix.command(Redix, ["SET", key(user_id), value(self())])
    :ok
  end

  @doc """
  Remove the presence-tracking pid for a given user
  """
  @spec remove(User.id()) :: :ok
  def remove(user_id) do
    {:ok, _} = Redix.command(Redix, ["DEL", key(user_id)])
    :ok
  end

  @doc """
  Get the active, presence-tracking pid for a given user
  """
  @spec get(User.id()) :: pid() | nil
  def get(user_id) do
    transaction(user_id, fn -> do_get(user_id) end)
  end

  def do_get(user_id) do
    case Redix.command(Redix, ["GET", key(user_id)]) do
      {:ok, nil} ->
        nil

      {:ok, pid_bin} ->
        pid_bin
        |> :erlang.binary_to_term()
        |> check_valid_presence(user_id)
    end
  end

  defp key(user_id), do: "presence_pid:" <> user_id

  defp value(pid), do: :erlang.term_to_binary(pid)

  defp check_valid_presence(pid, user_id) do
    if Process.alive?(pid) do
      pid
    else
      # Process is dead; take the opportunity to remove it
      remove(user_id)
      nil
    end
  end

  def transaction(user_id, fun) do
    case Process.get(:in_transaction) do
      true ->
        fun.()

      nil ->
        Process.put(:in_transaction, true)

        {:ok, result} =
          Redlock.transaction(
            "presence_manager:#{user_id}",
            @lock_timeout_secs,
            fn -> {:ok, fun.()} end
          )

        Process.put(:in_transaction, nil)
        result
    end
  end
end
