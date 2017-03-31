defmodule Wocky.Index do
  @moduledoc "Wocky interface to Algolia for full text search of users and bots"

  use ExActor.GenServer, export: :wocky_index

  require Logger
  alias Wocky.GeoUtils
  alias Wocky.User

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

  defstart start_link do
    user_indexes  = Application.fetch_env!(:wocky, :algolia_user_index_name)
    bot_indexes   = Application.fetch_env!(:wocky, :algolia_bot_index_name)
    inst_string   = Application.fetch_env!(:wocky, :wocky_inst)

    current_inst = :erlang.list_to_atom(inst_string)
    user_index = Keyword.get(user_indexes, current_inst)
    bot_index = Keyword.get(bot_indexes, current_inst)

    enabled = !is_nil(user_index) && !is_nil(bot_index)

    :ok = Logger.info("Indexing enabled: #{inspect(enabled)}")
    initial_state(
      %State{
        enabled: enabled,
        user_index: user_index,
        bot_index: bot_index
      }
    )
  end


  defcall geosearch(_, _), state: %State{enabled: false} do
    reply({:error, :no_index_configured})
  end

  defcall geosearch(lat, lon), state: %State{bot_index: index} do
    nlat = GeoUtils.normalize_degrees(lat)
    nlon = GeoUtils.normalize_degrees(lon)
    {:ok, result} =
      Algolia.search(index, <<>>,
                     %{aroundLatLng: "#{nlat},#{nlon}", getRankingInfo: true})

    bots = Enum.map(result["hits"], &object_to_bot/1)
    reply({:ok, bots})
  end

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

  defcall reindex(:users), state: %State{user_index: index} do
    User
    |> Repo.all
    |> Enum.each(
        fn (%User{username: user_id} = user) ->
          update_index(index, user_id, user, @user_fields)
        end)

    reply(:ok)
  end

  defcall reindex(:bots), state: %State{bot_index: _index} do
    # Bot
    # |> Repo.all
    # |> Enum.each(
    #     fn (%Bot{id: bot_id} = bot) ->
    #       update_index(index, bot_id, bot, @bot_fields)
    #     end)

    reply(:ok)
  end

  defcall reindex(_), state: %State{enabled: false} do
    reply(:ok)
  end

  @lint {Credo.Check.Readability.Specs, false}
  def handle_call(_, _, state) do
    {:reply, {:error, :bad_call}, state}
  end


  @lint {Credo.Check.Readability.Specs, false}
  def handle_cast(_msg, %State{enabled: false} = state) do
    {:noreply, state}
  end

  defcast user_updated(user_id, user), state: %State{user_index: index} do
    {:ok, _} = update_index(index, user_id, user, @user_fields)
    noreply()
  end

  defcast user_removed(user_id), state: %State{user_index: index} do
    {:ok, _} = delete_object(index, user_id)
    noreply()
  end

  defcast bot_updated(bot_id, bot), state: %State{bot_index: index} do
    {:ok, _} = update_index(index, bot_id, bot, @bot_fields)
    noreply()
  end

  defcast bot_removed(bot_id), state: %State{bot_index: index} do
    {:ok, _} = delete_object(index, bot_id)
    noreply()
  end

  @lint {Credo.Check.Readability.Specs, false}
  def handle_cast(msg, state) do
    # This is a little bit of hackery to stop the Elixir compiler from
    # complaining about the @lint attributes.
    _ = @lint

    :ok = Logger.warn("Unknown cast '#{inspect(msg)}'")
    {:noreply, state}
  end


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
