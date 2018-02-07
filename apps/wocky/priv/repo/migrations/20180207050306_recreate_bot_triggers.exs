defmodule Wocky.Repo.Migrations.RecreateBotTriggers do
  use Wocky.Repo.Migration

  alias Wocky.Repo.Migration.Utils

  @actions [:insert, :update, :delete]

  def up do
    Enum.each(@actions, &Utils.remove_notify("bots", &1))
    Enum.each(@actions, &Utils.add_notify("bots", &1, overrides()))
  end

  defp overrides, do: [{"location", "ST_AsGeoJSON($ITEM$, 20, 2)"}]
end
