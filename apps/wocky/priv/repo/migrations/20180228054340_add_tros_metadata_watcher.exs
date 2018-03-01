defmodule Wocky.Repo.Migrations.AddTrosMetadataWatcher do
  use Wocky.Repo.Migration

  alias Wocky.Repo.Migration.Utils

  def up do
    Utils.add_notify("tros_metadatas", [:update])
  end

  def down do
    Utils.remove_notify("tros_metadatas", [:update])
  end
end
