defmodule Wocky.Waiter do
  @moduledoc """
  Allows processes to do a semaphore-style wait on a given event
  """
  @type event() :: binary()

  @doc """
  Wait on an event. The `skip_callback` function is called after the wait
  is established. If it returns `true`, the wait will immediately be aborted
  and :ok will be returned. If the event is fired within the timeout, :ok
  is returned; otherwise :timeout is returned.
  """

  alias Timex.Duration

  # If a wait hasn't happened after a day then we can probably safely assume
  # it isn't going to happen. Let the redis entry expire at that point.
  @expire_secs Duration.from_days(1)
               |> Duration.to_seconds(truncate: true)
               |> to_string()

  @spec wait(event(), non_neg_integer | :infinity, (() -> boolean())) ::
          :ok | :timeout
  def wait(event, timeout, skip_callback) do
    ref = make_ref()
    key = key(event)
    val = val(self(), ref)
    {:ok, _} = Redix.command(Redix, ["SADD", key, val])
    {:ok, _} = Redix.command(Redix, ["EXPIRE", key, @expire_secs])

    result =
      case skip_callback.() do
        true -> :ok
        false -> do_wait(ref, timeout)
      end

    {:ok, _} = Redix.command(Redix, ["SREM", key, val])
    result
  end

  defp do_wait(ref, timeout) do
    receive do
      {:waiter_event, ^ref} -> :ok
    after
      timeout -> :timeout
    end
  end

  @doc "Notify all processes waiting on a the supplied event"
  @spec notify(event()) :: :ok
  def notify(event) do
    {:ok, waiters} = Redix.command(Redix, ["SMEMBERS", key(event)])
    Enum.each(waiters, &notify_waiter(&1))
  end

  defp notify_waiter(waiter) do
    {pid, ref} = :erlang.binary_to_term(waiter)
    send(pid, {:waiter_event, ref})
  end

  defp key(event), do: "wait:" <> event

  defp val(pid, ref), do: :erlang.term_to_binary({pid, ref})
end
