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
    defstruct [:backend, :indexes]
  end

  @type index_t :: :users | :bots
  @type object_t :: :user | :bot

  @fields [
    user: [:handle, :last_name, :first_name, :avatar],
    bot: [:server, :user_id, :title, :image, :lat, :lon, :radius, :public]
  ]

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

  @spec reindex(index_t) :: :ok | {:error, :unknown_index}
  def reindex(idx) do
    GenServer.call(:wocky_index, {:reindex, idx})
  end

  @spec update(object_t, binary, map) :: :ok
  def update(obj_t, id, fields) do
    GenServer.call(:wocky_index, {:update, obj_t, id, fields})
  end

  @spec remove(object_t, binary) :: :ok
  def remove(obj_t, id) do
    GenServer.call(:wocky_index, {:remove, obj_t, id})
  end

  # ===================================================================
  # Callbacks

  def init(_) do
    user_index = Confex.get(:wocky, :user_index_name, :users)
    bot_index  = Confex.get(:wocky, :bot_index_name, :bots)
    backend_type = Confex.get(:wocky, :indexing_system)
    backend_module =
      case backend_type do
        "algolia" -> Wocky.Index.AlgoliaIndexer
        "test" -> Wocky.Index.TestIndexer
      end

    backend_module.init()

    {:ok,
      %State{
        backend: backend_module,
        indexes: [
          user: user_index,
          bot: bot_index
        ]
      }
    }
  end

  def handle_call({:geosearch, lat, lon}, _, state) do
    nlat = lat |> GeoUtils.to_degrees |> GeoUtils.normalize_latitude
    nlon = lon |> GeoUtils.to_degrees |> GeoUtils.normalize_longitude
    result = state.backend.geosearch(state.indexes[:bot], nlat, nlon)
    {:reply, result, state}
  end

  def handle_call({:reindex, :users}, _, state) do
    index = state.indexes[:user]

    User
    |> Repo.all
    |> Enum.each(fn %User{id: id} = user ->
      update_index(index, id, user, @fields[:user], state.backend)
    end)

    {:reply, :ok, state}
  end

  def handle_call({:reindex, :bots}, _, state) do
    index = state.indexes[:bot]

    Bot
    |> Repo.all
    |> Enum.each(fn %Bot{id: id} = bot ->
      update_index(index, id, bot, @fields[:bot], state.backend)
    end)

    {:reply, :ok, state}
  end

  def handle_call({:reindex, _}, _, state) do
    {:reply, {:error, :unknown_index}, state}
  end

  def handle_call({:update, obj_t, id, fields}, _, state) do
    index = state.indexes[obj_t]
    update_index(index, id, fields, @fields[obj_t], state.backend)
    {:reply, :ok, state}
  end

  def handle_call({:remove, obj_t, id}, _, state) do
    delete_object(state.indexes[obj_t], id, state.backend)
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
