defmodule Wocky.Repo.Migrations.RemoveCollections do
  use Ecto.Migration

  def change do
    execute "DROP TRIGGER IF EXISTS delete_from_collections_trigger ON collections;"
    execute "DROP FUNCTION IF EXISTS delete_from_collections_trigger();"

    alter table(:home_stream_items) do
      remove :reference_collection_id
    end

    drop table(:collection_subscriptions)
    drop table(:collection_members)
    drop table(:collections)
  end
end
