defmodule Wocky.Repo.Migrations.AddMetadataCallback do
  use Wocky.Repo.Migration

  alias Wocky.Repo.Migration.Utils

  def up,
    do: Utils.update_notify("tros_metadatas", [:update])

  def down,
    do: Utils.remove_notify("tros_metadatas", [:update])

end
