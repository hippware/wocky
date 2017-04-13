defmodule Wocky.Repo.Migrations.AddRosterTables do
  use Ecto.Migration

  def up do
    drop table(:roster_version)
    drop table(:rostergroups)
    drop table(:rosterusers)

    create table(:roster_items) do
      add :user_id,      references(:users, type: :uuid, on_delete: :delete_all)
      add :contact,      references(:users, type: :uuid, on_delete: :delete_all)
      add :nick,         :string, null: false
      add :ask,          :string, null: false
      add :subscription, :string, null: false

      timestamps()
    end

    create unique_index(:roster_items, [:user_id, :contact])

    create table(:roster_groups) do
      add :roster_item_id, references(:roster_items, on_delete: :delete_all)
      add :name,           :string, null: false
    end
  end

  def down do
    drop table(:roster_groups)
    drop table(:roster_items)

    execute """
    CREATE TABLE rosterusers (\
    username varchar(250) NOT NULL, \
    jid varchar(250) NOT NULL, \
    nick text NOT NULL, \
    subscription character(1) NOT NULL, \
    ask character(1) NOT NULL, \
    askmessage text NOT NULL, \
    server character(1) NOT NULL, \
    subscribe text NOT NULL, \
    type text, \
    created_at timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP\
    ) CHARACTER SET utf8;\
    """
    execute """
    CREATE UNIQUE INDEX i_rosteru_user_jid ON\
     rosterusers(username(75), jid(75));\
    """
    execute "CREATE INDEX i_rosteru_username ON rosterusers(username);"
    execute "CREATE INDEX i_rosteru_jid ON rosterusers(jid);"

    execute """
    CREATE TABLE rostergroups (\
    username varchar(250) NOT NULL, \
    jid varchar(250) NOT NULL, \
    grp text NOT NULL\
    ) CHARACTER SET utf8;\
    """
    execute """
    CREATE INDEX pk_rosterg_user_jid ON rostergroups(username(75), jid(75));\
    """

    execute """
    CREATE TABLE roster_version (\
    username varchar(250) PRIMARY KEY, \
    version text NOT NULL\
    ) CHARACTER SET utf8;\
    """
  end

end
