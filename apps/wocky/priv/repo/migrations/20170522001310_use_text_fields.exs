defmodule Wocky.Repo.Migrations.UseTextFields do
  use Ecto.Migration

  def change do
    alter table(:bots) do
      modify :description, :text
    end

    alter table(:conversations) do
      modify :message, :text, null: false
    end

    alter table(:tros_metadatas) do
      modify :access, :text, null: false
    end

    alter table(:traffic_logs) do
      modify :packet, :text, null: false
    end

    alter table(:home_stream_items) do
      modify :stanza, :text, null: false
    end
  end

end
