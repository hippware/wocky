defmodule Wocky.Repo.Migrations.RemoveOrderingField do
  use Wocky.Repo.Migration
  use Wocky.Repo.Model

  alias Wocky.HomeStreamItem

  def up do
    alter table(:home_stream_items) do
      remove :ordering
    end
  end

  def down do
    alter table(:home_stream_items) do
      add :ordering, :timestamptz
    end

    flush()

    HomeStreamItem
    |> update([i], set: [ordering: i.updated_at])
    |> Repo.update_all([])

    alter table(:home_stream_items) do
      modify :ordering, :timestamptz, null: false
    end
  end

end
