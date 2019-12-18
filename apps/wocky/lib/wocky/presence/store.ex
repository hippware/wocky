defmodule Wocky.Presence.Store do
  @moduledoc """
  Redis-based store for user_id -> [presence_pid] mappings
  """

  alias Timex.Duration
  alias Wocky.Account.User
  alias Wocky.Errors

  @lock_timeout_secs 5

  # Expire untouched records after two weeks - far longer than any TCP
  # connection is likely to be up for.
  @expire_secs Duration.from_weeks(2)
               |> Duration.to_seconds(truncate: true)
               |> to_string()

  @doc """
  Add the calling presence-tracking process to the records for a user
  """
  @spec add_self(User.tid()) :: :ok | {:error, any()}
  def add_self(user) do
    with {:ok, _} <- do_add_self(user) do
      :ok
    end
  end

  defp do_add_self(user) do
    Redix.command(Redix, [
      "SET",
      key(user),
      value(self()),
      "EX",
      @expire_secs
    ])
  end

  @spec set_self_online(User.tid(), pid()) :: :ok | {:error, any()}
  def set_self_online(user, online_pid) do
    with {:ok, _} <- do_set_self_online(user, online_pid) do
      :ok
    end
  end

  defp do_set_self_online(user, online_pid) do
    Redix.command(Redix, [
      "SET",
      key(user),
      value(self(), online_pid),
      "EX",
      @expire_secs
    ])
  end

  @doc """
  Remove the presence-tracking pid for a given user
  """
  @spec remove(User.tid()) :: :ok | {:error, any()}
  def remove(user) do
    with {:ok, _} <- Redix.command(Redix, ["DEL", key(user)]) do
      :ok
    end
  end

  @doc """
  Get the active, presence-tracking pid for a given user
  """
  @spec get_manager(User.tid()) :: {:ok, pid() | nil} | {:error, any()}
  def get_manager(user) do
    transaction(user, fn -> do_get_manager(user) end)
  end

  defp do_get_manager(user) do
    case Redix.command(Redix, ["GET", key(user)]) do
      {:ok, nil} ->
        {:ok, nil}

      {:ok, pid_bin} ->
        pid_bin
        |> :erlang.binary_to_term()
        |> check_valid_manager(user)

      {:error, _} = error ->
        error
    end
  end

  @spec get_online(User.tid()) :: {:ok, pid() | nil} | {:error, any()}
  def get_online(user) do
    transaction(user, fn -> do_get_online(user) end)
  end

  defp do_get_online(user) do
    case Redix.command(Redix, ["GET", key(user)]) do
      {:ok, nil} ->
        {:ok, nil}

      {:ok, pid_bin} ->
        pid_bin
        |> :erlang.binary_to_term()
        |> check_valid_online()

      {:error, _} = error ->
        error
    end
  end

  defp key(user), do: "presence_pid:" <> User.id(user)

  defp value(pid), do: :erlang.term_to_binary(pid)

  defp value(pid, online_pid), do: :erlang.term_to_binary({pid, online_pid})

  defp check_valid_manager({pid, _online_pid}, user),
    do: check_valid_manager(pid, user)

  defp check_valid_manager(pid, user) when is_pid(pid) do
    case remote_alive?(pid) do
      true ->
        {:ok, pid}

      _ ->
        # Process is dead or node is unreachable;
        # take the opportunity to remove it
        remove_quietly(user)
        {:ok, nil}
    end
  end

  # process_id could not be parsed back to a valid pid - maybe a node died or
  # there was a version upgrade or a bug. Whatever, we can't do anything with
  # it besides clean up.
  defp check_valid_manager(_, user) do
    remove_quietly(user)
    {:ok, nil}
  end

  defp remove_quietly(user) do
    Errors.log_on_failure(
      "Removing Redis presence data for user #{User.id(user)}",
      fn -> remove(user) end
    )
  end

  defp check_valid_online({_pid, online_pid}) do
    case remote_alive?(online_pid) do
      true ->
        {:ok, online_pid}

      _ ->
        {:ok, nil}
    end
  end

  defp check_valid_online(_), do: nil

  @spec transaction(User.tid(), fun()) :: any()
  def transaction(user, fun) do
    case Process.get(:in_transaction) do
      true ->
        fun.()

      nil ->
        Process.put(:in_transaction, true)

        {:ok, result} =
          Redlock.transaction(
            "presence_manager:#{User.id(user)}",
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
