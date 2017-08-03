defmodule Wocky.Repo.Migrations.RadiusToFloat do
  use Ecto.Migration

  def up do
    alter table(:bots) do
      modify :radius, :float
    end

    execute """
    UPDATE bots SET radius = radius / 1000
    """
  end

  def down do
    execute """
    UPDATE bots SET radius = radius * 1000
    """

    alter table(:bots) do
      modify :radius, :integer
    end
  end
end
