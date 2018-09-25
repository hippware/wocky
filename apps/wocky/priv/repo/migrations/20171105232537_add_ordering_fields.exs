defmodule Wocky.Repo.Migrations.AddOrderingFields do
  use Wocky.Repo.Migration

  def up do
    alter table(:home_stream_items) do
      add :ordering, :timestamp
    end

    flush()

    execute "UPDATE home_stream_items SET ordering = updated_at"

    alter table(:home_stream_items) do
      modify :ordering, :timestamp, null: false
    end
  end

  def down do
    alter table(:home_stream_items) do
      remove :ordering
    end
  end
end
