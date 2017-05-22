defmodule Wocky.Index do
  @moduledoc "Wocky interface to Algolia for full text search of users and bots"

  use GenServer

  alias Wocky.Bot
  alias Wocky.GeoUtils
  alias Wocky.Repo
  alias Wocky.User

  require Logger

  defmodule State do
    @moduledoc false
    defstruct [:backend, :user_index, :bot_index]
  end

  @user_fields [:handle, :last_name, :first_name, :avatar]
  @bot_fields [:server, :owner, :title, :image, :lat, :lon, :radius]

  # ===================================================================
  # Behaviour definition

  @callback init :: :ok
  @callback update_object(index :: binary, id :: binary, map :: map) :: :ok
  @callback delete_object(index :: binary, id :: binary) :: :ok
  @callback geosearch(index :: binary, lat :: float, lon :: float) ::
    {:ok, any} | {:error, any}

  # ===================================================================
  # API

  @spec start_link :: {:ok, pid} | {:error, any}
  def start_link do
    GenServer.start_link(__MODULE__, [], name: :wocky_index)
  end

  @spec geosearch(float, float) :: {:ok, list} | {:error, any}
  def geosearch(lat, lon) do
    GenServer.call(:wocky_index, {:geosearch, lat, lon})
  end

  @spec reindex(:users | :bots) :: :ok | {:error, :unknown_index}
  def reindex(idx) do
    GenServer.call(:wocky_index, {:reindex, idx})
  end

  @spec user_updated(User.id, map) :: :ok
  def user_updated(user_id, fields) do
    GenServer.call(:wocky_index, {:user_updated, user_id, fields})
  end

  @spec user_removed(User.id) :: :ok
  def user_removed(user_id) do
    GenServer.call(:wocky_index, {:user_removed, user_id})
  end

  @spec bot_updated(Bot.id, map) :: :ok
  def bot_updated(bot_id, fields) do
    GenServer.call(:wocky_index, {:bot_updated, bot_id, fields})
  end

  @spec bot_removed(Bot.id) :: :ok
  def bot_removed(bot_id) do
    GenServer.call(:wocky_index, {:bot_removed, bot_id})
  end

  # ===================================================================
  # Callbacks

  def init(_) do
    user_index = Confex.get(:wocky, :user_index_name, :users)
    bot_index  = Confex.get(:wocky, :bot_index_name, :bots)
    backend_type = Confex.get(:wocky, :indexing_system, "none")
    backend_module =
      case backend_type do
        "algolia" -> Wocky.Index.AlgoliaIndexer
        "test" -> Wocky.Index.TestIndexer
        "none" -> Wocky.Index.NullIndexer
      end

    backend_module.init()

    {:ok,
      %State{
        backend: backend_module,
        user_index: user_index,
        bot_index: bot_index
      }
    }
  end

  def handle_call({:geosearch, lat, lon}, _, state) do
    nlat = lat |> GeoUtils.to_degrees |> GeoUtils.normalize_latitude
    nlon = lon |> GeoUtils.to_degrees |> GeoUtils.normalize_longitude
    result = state.backend.geosearch(state.bot_index, nlat, nlon)
    {:reply, result, state}
  end

  def handle_call({:reindex, :users}, _, state) do
    User
    |> Repo.all
    |> Enum.each(fn %User{username: user_id} = user ->
      update_index(state.user_index, user_id, user, @user_fields, state.backend)
    end)

    {:reply, :ok, state}
  end

  def handle_call({:reindex, :bots}, _, state) do
    Bot
    |> Repo.all
    |> Enum.each(fn %Bot{id: bot_id} = bot ->
      update_index(state.bot_index, bot_id, bot, @bot_fields, state.backend)
    end)

    {:reply, :ok, state}
  end

  def handle_call({:reindex, _}, _, state) do
    {:reply, {:error, :unknown_index}, state}
  end

  def handle_call({:user_updated, user_id, user}, _, state) do
    update_index(state.user_index, user_id, user, @user_fields, state.backend)
    {:reply, :ok, state}
  end

  def handle_call({:user_removed, user_id}, _, state) do
    delete_object(state.user_index, user_id, state.backend)
    {:reply, :ok, state}
  end

  def handle_call({:bot_updated, bot_id, bot}, _, state) do
    update_index(state.bot_index, bot_id, bot, @bot_fields, state.backend)
    {:reply, :ok, state}
  end

  def handle_call({:bot_removed, bot_id}, _, state) do
    delete_object(state.bot_index, bot_id, state.backend)
    {:reply, :ok, state}
  end

  def handle_call(_, _, state) do
    {:reply, {:error, :bad_call}, state}
  end

  def handle_cast(msg, state) do
    :ok = Logger.warn("Unknown cast '#{inspect(msg)}'")
    {:noreply, state}
  end

  # ===================================================================
  # Helpers

  defp update_index(index, id, data, fields, backend) do
    data
    |> Map.take(fields)
    |> Enum.reject(fn {_k, v} -> v == nil end)
    |> Enum.into(%{}, fn {k, v} -> {to_string(k), v} end)
    |> do_update_index(id, index, backend)
  end

  defp do_update_index(object, id, index, backend) do
    :ok = Logger.debug("Updating #{index} index with object #{inspect(object)}")
    if map_size(object) < 1 do
      {:ok, :no_changes}
    else
      backend.update_object(index, id, object)
    end
  end

  defp delete_object(index, id, backend) do
    :ok = Logger.debug("Removing object #{id} from #{index}")
    backend.delete_object(index, id)
  end
end
