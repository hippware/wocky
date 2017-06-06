defmodule Wocky.Repo.Migrations.RemoveBotNulls do
  use Ecto.Migration

  def change do
    alter table(:bots) do
      modify :title,       :string, null: false, default: ""
      modify :shortname,   :string, null: false, default: ""
      modify :address,     :string, null: false, default: ""
      modify :type,        :string, null: false, default: ""
      modify :description, :text,   null: false, default: ""
    end
  end
end
