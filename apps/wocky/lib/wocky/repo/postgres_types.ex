Postgrex.Types.define(Wocky.Repo.PostgresTypes,
                      [Geo.PostGIS.Extension
                       | Ecto.Adapters.Postgres.extensions()],
                      json: Poison)
