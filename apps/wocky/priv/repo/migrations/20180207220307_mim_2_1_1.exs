defmodule Wocky.Repo.Migrations.MIM211 do
  use Ecto.Migration

  def change do
    alter table("mam_message") do
      add :search_body, :text
    end

    alter table("mam_muc_message") do
      add :search_body, :text
    end
  end
end
