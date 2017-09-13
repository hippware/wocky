defmodule Wocky.Repo.Migrations.BotAddressData do
  use Ecto.Migration

  def change do
    alter table(:bots) do
      add :address_data, :text
    end
  end
end
