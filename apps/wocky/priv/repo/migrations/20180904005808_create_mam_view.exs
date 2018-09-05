defmodule Wocky.Repo.Migrations.CreateMamView do
  use Ecto.Migration

  def up do
    execute """
    CREATE VIEW messages
      (id, user_id, other_user_id, incoming, message) AS
    SELECT
      mam_message.id,
      uuid(mam_server_user.user_name),
      uuid(mam_message.remote_bare_jid),
      mam_message.direction = 'I',
      mam_message.message
    FROM mam_message
      INNER JOIN mam_server_user
      ON mam_message.user_id = mam_server_user.id
    WHERE mam_message.remote_bare_jid != ''
    """
  end

  def down do
    execute "DROP VIEW messages"
  end
end
