defmodule Wocky.Repo.Migrations.AddHsItemCallback do
  use Ecto.Migration

  alias Wocky.Repo.Migration.Utils

  def up do
    Utils.add_notify("home_stream_items", [:insert, :update])
  end

  def down do
    Utils.remove_notify("home_stream_items", [:insert, :update])
  end
end
