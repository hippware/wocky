defmodule Wocky.Repo.Migrations.TrosImageFlags do
  use Wocky.Repo.Migration

  alias Wocky.TROS.Metadata.FileTypeEnum

  @disable_ddl_transaction true

  def up do
    FileTypeEnum.create_type()

    alter table(:tros_metadatas) do
      add :available_formats, {:array, FileTypeEnum.type()},
        default: ["full", "original", "thumbnail"]
    end
  end
end
