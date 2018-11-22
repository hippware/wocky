defmodule Wocky.Repo.Migrations.RemoveXmpp do
  use Ecto.Migration

  def change do
    Enum.each(["bots", "bot_items", "users"], &drop_hs_del_triggers/1)
    drop table ("home_stream_items")
    drop table ("mam_config")
    drop table ("mam_message")
    drop table ("mam_muc_message")
    drop table ("mam_server_user")
    drop table ("privacy_default_list")
    drop table ("privacy_list")
    drop table ("privacy_list_data")
    drop table ("private_storage")

    alter table("users") do
      remove :username
    end
  end

  defp drop_hs_del_triggers(t) do
    execute "DROP TRIGGER delete_from_#{t}_trigger ON #{t}"
    execute "DROP FUNCTION delete_from_#{t}_trigger()"
  end
end
