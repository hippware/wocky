defmodule Wocky.Push.Sandbox do
  @moduledoc """
  Sandbox where notifications get saved when the application is running in
  sandbox mode. This is meant to be used in tests, and should not be used in
  production.  Note that all operations are dependent on the `pid`, so the
  process calling and `Push.notify/2` and the process calling
  `Sandbox.list_notifications/1` must be the same, or the `pid` should be
  passed explicitly otherwise.
  """

  use GenServer

  alias Pigeon.APNS.Notification

  @doc """
  Records the notification. This is used to record
  requests and responses.
  """
  @spec record_notification(%Notification{}, pid) :: :ok
  def record_notification(notification, pid) do
    GenServer.call(__MODULE__, {:record_notification, notification, pid})
  end

  @doc """
  Wait until a notification arrives.
  """
  @spec wait_notifications(
          pid: pid,
          global: boolean,
          timeout: non_neg_integer,
          count: non_neg_integer
        ) :: [%Notification{}]
  def wait_notifications(opts \\ []) do
    timeout = opts[:timeout] || 100
    count = opts[:count] || 1

    case list_notifications(opts) do
      notifications when length(notifications) < count and timeout > 0 ->
        receive do
        after
          10 ->
            wait_notifications(Keyword.put(opts, :timeout, timeout - 10))
        end

      notifications ->
        notifications
    end
  end

  @doc """
  List recorded notifications keeping their order of arrival.
  """
  @spec list_notifications(pid: pid, global: boolean) :: [%Notification{}]
  def list_notifications(opts \\ []) do
    pid = opts[:pid] || self()
    GenServer.call(__MODULE__, {:list_notifications, pid, opts[:global]})
  end

  @doc """
  Clear all the recorded notifications.
  """
  @spec clear_notifications(pid: pid, global: boolean) :: :ok
  def clear_notifications(opts \\ []) do
    pid = opts[:pid] || self()
    GenServer.call(__MODULE__, {:clear_notifications, pid, opts[:global]})
  end

  @doc false
  def start_link do
    GenServer.start_link(__MODULE__, %{}, name: __MODULE__)
  end

  @doc false
  def init(args) do
    {:ok, args}
  end

  @doc false
  def handle_call({:record_notification, n, pid}, _from, state) do
    notifications = [n | Map.get(state, pid, [])]
    {:reply, :ok, Map.put(state, pid, notifications)}
  end

  @doc false
  def handle_call({:list_notifications, pid, global}, _from, state) do
    notifications =
      if global do
        state |> Map.values() |> List.flatten()
      else
        state |> Map.get(pid, []) |> Enum.reverse()
      end

    {:reply, notifications, state}
  end

  @doc false
  def handle_call({:clear_notifications, pid, global}, _from, state) do
    if global do
      {:reply, :ok, %{}}
    else
      {:reply, :ok, Map.put(state, pid, [])}
    end
  end
end
