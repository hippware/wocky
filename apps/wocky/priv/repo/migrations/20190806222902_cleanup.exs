defmodule Wocky.Repo.Migrations.Cleanup do
  use Wocky.Repo.Migration

  def up do
    # Cleanup some database structures that are left behind from previous
    # versions of the schema that are no longer used.

    execute "DROP TYPE IF EXISTS behaviour"
    execute "DROP TYPE IF EXISTS direction"

    execute "DROP FUNCTION IF EXISTS notify_bot_shares_insert()"
    execute "DROP FUNCTION IF EXISTS notify_home_stream_items_insert()"
    execute "DROP FUNCTION IF EXISTS notify_home_stream_items_update()"
    execute "DROP FUNCTION IF EXISTS notify_mam_message_insert()"

    execute "DROP SEQUENCE IF EXISTS mam_server_user_id_seq"
    execute "DROP SEQUENCE IF EXISTS offline_id_seq"
    execute "DROP SEQUENCE IF EXISTS privacy_list_id_seq"
  end
end
