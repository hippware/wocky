defmodule Wocky.Presence.Store do
  @moduledoc """
  Redis-based store for user_id -> [presence_pid] mappings
  """

  alias Timex.Duration
  alias Wocky.Account.User

  @lock_timeout_secs 5

  # Expire untouched records after two weeks - far longer than any TCP
  # connection is likely to be up for.
  @expire_secs Duration.from_weeks(2)
               |> Duration.to_seconds(truncate: true)
               |> to_string()

  @doc """
  Add the calling presence-tracking process to the records for a user
  """
  @spec add_self(User.id()) :: :ok
  def add_self(user_id) do
    {:ok, _} =
      Redix.command(Redix, [
        "SET",
        key(user_id),
        value(self()),
        "EX",
        @expire_secs
      ])

    :ok
  end

  @spec set_self_online(User.id(), pid()) :: :ok
  def set_self_online(user_id, online_pid) do
    {:ok, _} =
      Redix.command(Redix, [
        "SET",
        key(user_id),
        value(self(), online_pid),
        "EX",
        @expire_secs
      ])

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
  @spec get_manager(User.id()) :: pid() | nil
  def get_manager(user_id) do
    transaction(user_id, fn -> do_get_manager(user_id) end)
  end

  def do_get_manager(user_id) do
    case Redix.command(Redix, ["GET", key(user_id)]) do
      {:ok, nil} ->
        nil

      {:ok, pid_bin} ->
        pid_bin
        |> :erlang.binary_to_term()
        |> check_valid_manager(user_id)
    end
  end

  @spec get_online(User.id()) :: pid() | nil
  def get_online(user_id) do
    transaction(user_id, fn -> do_get_online(user_id) end)
  end

  def do_get_online(user_id) do
    case Redix.command(Redix, ["GET", key(user_id)]) do
      {:ok, nil} ->
        nil

      {:ok, pid_bin} ->
        pid_bin
        |> :erlang.binary_to_term()
        |> check_valid_online()
    end
  end

  defp key(user_id), do: "presence_pid:" <> user_id

  defp value(pid), do: :erlang.term_to_binary(pid)

  defp value(pid, online_pid), do: :erlang.term_to_binary({pid, online_pid})

  defp check_valid_manager({pid, _online_pid}, user_id),
    do: check_valid_manager(pid, user_id)

  defp check_valid_manager(pid, user_id) when is_pid(pid) do
    case remote_alive?(pid) do
      true ->
        pid

      _ ->
        # Process is dead or node is unreachable;
        # take the opportunity to remove it
        remove(user_id)
        nil
    end
  end

  # process_id could not be parsed back to a valid pid - maybe a node died or
  # there was a version upgrade or a bug. Whatever, we can't do anything with
  # it besides clean up.
  defp check_valid_manager(_, user_id) do
    remove(user_id)
    nil
  end

  defp check_valid_online({_pid, online_pid}) do
    case remote_alive?(online_pid) do
      true ->
        online_pid

      _ ->
        nil
    end
  end

  defp check_valid_online(_), do: nil

  @spec transaction(User.id(), fun()) :: any()
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

  defp remote_alive?(pid),
    do: :rpc.call(node(pid), Process, :alive?, [pid], 2000)
end
