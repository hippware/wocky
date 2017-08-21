defmodule Wocky.Auth.FirebaseKeyManager do
  require Logger

  use GenServer

  @key_url "https://www.googleapis.com/robot/v1/metadata/x509/securetoken@system.gserviceaccount.com"

  def start_link() do
    GenServer.start_link(__MODULE__, [], name: __MODULE__)
  end

  def get_key(id) do
    case :ets.lookup(:firebase_keys, id) do
      [{^id, key}] -> {:ok, key}
      [] -> {:error, :no_key}
    end
  end

  def force_reload() do
    GenServer.call(__MODULE__, :reload_keys)
  end

  def init(_) do
    _ = :ets.new(:firebase_keys, [:protected, :named_table])
    if Confex.get(:wocky, :firebase_load_on_startup, true) do
      reload_keys()
    end
    {:ok, nil}
  end

  def handle_call(:reload_keys, _from, _state) do
    reload_keys()
    {:reply, :ok, nil}
  end

  def handle_info(:reload_keys, _state) do
    reload_keys()
    {:noreply, nil}
  end

  defp reload_keys() do
    case :hackney.get(@key_url, [], "", []) do
      {:ok, 200, headers, client} ->
        update_keys(headers, client)
      other ->
        :ok = Logger.warn("Error getting Firebase keys: #{inspect other}")
        set_reload(10)
    end
  end

  defp update_keys(headers, client) do
    {:ok, body} = :hackney.body(client)
    keys = Poison.Parser.parse!(body)
    Enum.each(keys, &:ets.insert(:firebase_keys, &1))
    remove_old_keys(keys)
    set_reload_from_header(headers)
  end

  defp remove_old_keys(keys) do
    all_keys =
      :firebase_keys
      |> :ets.tab2list()
      |> Enum.unzip
      |> elem(0)

    new_keys =
      keys
      |> Enum.unzip
      |> elem(0)

    expired_keys = all_keys -- new_keys
    Enum.each(expired_keys, &:ets.delete(:firebase_keys, &1))
  end

  defp set_reload_from_header(headers) do
    "Cache-Control"
    |> :proplists.get_value(headers)
    |> String.split(", ")
    |> Enum.find(&String.match?(&1, ~r/max-age=.*/))
    |> String.split("=")
    |> Enum.at(1)
    |> String.to_integer
    |> set_reload()
  end

  defp set_reload(seconds) do
    :erlang.send_after(:timer.seconds(seconds), self(), :reload_keys)
  end

end
