defmodule Wocky.Repo.Migrations.AddOrderingFields do
  use Wocky.Repo.Migration
  use Wocky.Repo.Model

  alias Wocky.HomeStreamItem

  def up do
    alter table(:home_stream_items) do
      add :ordering, :timestamp
    end

    flush()

    HomeStreamItem
    |> update([i], set: [ordering: i.updated_at])
    |> Repo.update_all([])

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
