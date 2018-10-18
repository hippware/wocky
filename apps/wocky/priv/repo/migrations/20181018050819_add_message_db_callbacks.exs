defmodule Wocky.Repo.Migrations.AddMessageDbCallbacks do
  use Wocky.Repo.Migration

  alias Wocky.Repo.Migration.Utils

  def up,
    do: Utils.update_notify("mam_message", [:insert])

  def down,
    do: Utils.remove_notify("mam_message", [:insert])

end
