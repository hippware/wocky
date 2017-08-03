defmodule Wocky.Repo.Migrations.AddPostgis do
  use Wocky.Repo.Migration

  import Ecto.Query

  alias Ecto.UUID
  alias Geo.Point
  alias Wocky.Repo

  def up do
    execute "CREATE EXTENSION postgis;"
    execute "CREATE EXTENSION postgis_topology;"

    execute "ALTER TABLE BOTS ADD COLUMN location geography(Point,4326);"

    flush()

    from(b in "bots",
         select: %{:id => b.id,
                   :lat => b.lat,
                   :lon => b.lon})
    |> Repo.stream
    |> Stream.each(&set_location/1)
    |> Enum.to_list

    alter table(:bots) do
      remove :lat
      remove :lon
    end
  end

  def down do
    from(b in "bots",
         select: %{:id => b.id,
                   :location => b.location})
    |> Repo.stream
    |> Stream.each(&set_lat_lon/1)
    |> Enum.to_list

    alter table(:bots) do
      remove :location
    end

    execute "DROP EXTENSION postgis_topology;"
    execute "DROP EXTENSION postgis;"
  end

  defp set_location(%{id: id, lat: lat, lon: lon}) do
    {:ok, id_str} = UUID.load(id)

    # Unfortunately we can't use the pretty Ecto functions here because for
    # some reason it doesn't load the OID for the POINT type until the next
    # restart. So we have to do things the old fashioned way:
    execute "UPDATE bots SET location = 'POINT(#{lon} #{lat})' WHERE id = '#{id_str}';"
  end

  defp set_lat_lon(%{id: id, location: %Point{coordinates: {lon, lat}}}) do
    from(b in "bots",
         update: [set: [lat: ^lat, lon: ^lon]],
         where: [id: ^id, pending: false])
    |> Repo.update_all([])
  end
end
