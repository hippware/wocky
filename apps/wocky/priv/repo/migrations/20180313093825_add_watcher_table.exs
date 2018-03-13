defmodule Wocky.Repo.Migrations.AddWatcherTable do
  use Wocky.Repo.Migration

  alias Wocky.Repo.Migration.Utils

  @bot_overrides [{"location", "ST_AsGeoJSON($ITEM$, 20, 2)"}]

  @triggers [{"bots", [:insert, :update, :delete], @bot_overrides},
             {"bot_shares", [:insert], []},
             {"home_stream_items", [:insert, :update], []},
             {"bot_items", [:insert, :delete], []},
             {"tros_metadatas", [:update], []}]

  def up do
    create table("watcher_events") do
      add :payload, :text, null: false

      Wocky.Repo.Migration.timestamps(updated_at: false)
    end

    Enum.each(@triggers,
              fn {table, actions, overrides} ->
                Enum.each(actions, &Utils.update_notify(table, &1, overrides))
              end)
  end
end
