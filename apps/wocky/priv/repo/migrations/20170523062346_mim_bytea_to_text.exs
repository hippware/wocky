defmodule Wocky.Repo.Migrations.MimByteaToText do
  use Ecto.Migration

  def change do
    alter table(:offline_message) do
      modify :packet, :text
    end

    alter table(:mam_message) do
      modify :message, :text
    end

    alter table(:mam_muc_message) do
      modify :message, :text
    end
  end
end
