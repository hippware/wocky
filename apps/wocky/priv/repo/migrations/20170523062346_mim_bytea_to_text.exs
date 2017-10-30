defmodule Wocky.Repo.Migrations.MimByteaToText do
  use Wocky.Repo.Migration

  def change do
    alter table(:offline_message) do
      modify :packet, :text
    end
  end
end
