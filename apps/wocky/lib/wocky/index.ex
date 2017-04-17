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
    defstruct [
      enabled:    false,
      user_index: nil,
      bot_index:  nil
    ]
  end

  @user_fields [:handle, :last_name, :first_name, :avatar]
  @bot_fields [:server, :owner, :title, :image, :lat, :lon, :radius, :_geoloc]

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
    GenServer.cast(:wocky_index, {:user_updated, user_id, fields})
  end

  @spec user_removed(User.id) :: :ok
  def user_removed(user_id) do
    GenServer.cast(:wocky_index, {:user_removed, user_id})
  end

  @spec bot_updated(Bot.id, map) :: :ok
  def bot_updated(bot_id, fields) do
    GenServer.cast(:wocky_index, {:bot_updated, bot_id, fields})
  end

  @spec bot_removed(Bot.id) :: :ok
  def bot_removed(bot_id) do
    GenServer.cast(:wocky_index, {:bot_removed, bot_id})
  end

  # Callbacks

  def init(_) do
    user_index = Application.fetch_env!(:wocky, :algolia_user_index_name)
    bot_index  = Application.fetch_env!(:wocky, :algolia_bot_index_name)

    enabled = !is_nil(user_index) && !is_nil(bot_index)

    :ok = Logger.info("Indexing enabled: #{inspect(enabled)}")
    {:ok,
      %State{
        enabled: enabled,
        user_index: user_index,
        bot_index: bot_index
      }
    }
  end


  def handle_call({:geosearch, _, _}, _, %State{enabled: false} = state) do
    {:reply, {:error, :no_index_configured}, state}
  end

  def handle_call({:geosearch, lat, lon}, _, state) do
    nlat = GeoUtils.normalize_latitude(lat)
    nlon = GeoUtils.normalize_longitude(lon)
    {:ok, result} = Algolia.search(state.bot_index, <<>>, %{
                                     aroundLatLng: "#{nlat},#{nlon}",
                                     getRankingInfo: true
                                   })

    bots = Enum.map(result["hits"], &object_to_bot/1)
    {:reply, {:ok, bots}, state}
  end

  def handle_call({:reindex, _}, _, %State{enabled: false} = state) do
    {:reply, :ok, state}
  end

  def handle_call({:reindex, :users}, _, state) do
    User
    |> Repo.all
    |> Enum.each(
        fn (%User{username: user_id} = user) ->
          update_index(state.user_index, user_id, user, @user_fields)
        end)

    {:reply, :ok, state}
  end

  def handle_call({:reindex, :bots}, _, state) do
    # Bot
    # |> Repo.all
    # |> Enum.each(
    #     fn (%Bot{id: bot_id} = bot) ->
    #       update_index(state.bot_index, bot_id, bot, @bot_fields)
    #     end)

    {:reply, :ok, state}
  end

  def handle_call({:reindex, _}, _, state) do
    {:reply, {:error, :unknown_index}, state}
  end

  def handle_call(_, _, state) do
    {:reply, {:error, :bad_call}, state}
  end


  def handle_cast(_msg, %State{enabled: false} = state) do
    {:noreply, state}
  end

  def handle_cast({:user_updated, user_id, user}, state) do
    {:ok, _} = update_index(state.user_index, user_id, user, @user_fields)
    {:noreply, state}
  end

  def handle_cast({:user_removed, user_id}, state) do
    {:ok, _} = delete_object(state.user_index, user_id)
    {:noreply, state}
  end

  def handle_cast({:bot_updated, bot_id, bot}, state) do
    {:ok, _} = update_index(state.bot_index, bot_id, bot, @bot_fields)
    {:noreply, state}
  end

  def handle_cast({:bot_removed, bot_id}, state) do
    {:ok, _} = delete_object(state.bot_index, bot_id)
    {:noreply, state}
  end

  def handle_cast(msg, state) do
    :ok = Logger.warn("Unknown cast '#{inspect(msg)}'")
    {:noreply, state}
  end

  # Helpers

  defp update_index(index, id, data, fields) do
    {:ok, _} =
      data
      |> map_to_object(id, fields)
      |> do_update_index(index)
  end

  defp map_to_object(map, id, fields) do
    map
    |> with_geoloc
    |> Map.take(fields)
    |> Map.put(:objectID, id)
    |> Enum.reject(fn {_k, v} -> v == nil end)
    |> Enum.into(%{}, fn {k, v} -> {to_string(k), v} end)
  end

  defp with_geoloc(%{lat: lat, lon: lon} = data) do
    Map.put(data, :_geoloc, %{lat: lat, lng: lon})
  end
  defp with_geoloc(data), do: data

  defp object_to_bot(obj) do
    %{
      id: obj["objectID"],
      server: obj["server"],
      owner: obj["owner"],
      title: obj["title"],
      image: obj["image"],
      lat: obj["lat"],
      lon: obj["lon"],
      radius: obj["radius"],
      distance: obj["_rankingInfo"]["geoDistance"] * 1000 # millimeters
     }
  end

  defp do_update_index(object, index) do
    :ok = Logger.debug("Updating #{index} index with object #{inspect(object)}")
    if map_size(object) < 1 do
      {:ok, :no_changes}
    else
      Algolia.partial_update_objects(index, [object])
    end
  end

  defp delete_object(index, id) do
    :ok = Logger.debug("Removing object #{id} from #{index}")
    {:ok, _} = Algolia.delete_object(index, id)
  end
end
