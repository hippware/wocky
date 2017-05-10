defmodule Wocky.Repo.Migrations.MIMSchema do
  use Wocky.Repo.Migration

  # This migration creates the default MongooseIM schema. The MIM schema is
  # a little weird from Ecto's perspective, so I have used `execute/1` with
  # SQL statements to ensure that we get the schema that MIM expects. Note
  # that we don't use the vcard module, so I have not included its tables here.

  def up do
    execute """
    CREATE TABLE users (\
    username varchar(250) PRIMARY KEY, \
    password text NOT NULL, \
    pass_details text, \
    created_at timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP\
    ) ;\
    """

    execute """
    CREATE TABLE last (\
    username varchar(250) PRIMARY KEY, \
    seconds int NOT NULL, \
    state text NOT NULL\
    ) ;\
    """
    execute "CREATE INDEX i_last_seconds ON last(seconds);"

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
    ) ;\
    """
    execute """
    CREATE UNIQUE INDEX i_rosteru_user_jid ON\
     rosterusers(username, jid);\
    """
    execute "CREATE INDEX i_rosteru_username ON rosterusers(username);"
    execute "CREATE INDEX i_rosteru_jid ON rosterusers(jid);"

    execute """
    CREATE TABLE rostergroups (\
    username varchar(250) NOT NULL, \
    jid varchar(250) NOT NULL, \
    grp text NOT NULL\
    ) ;\
    """
    execute """
    CREATE INDEX pk_rosterg_user_jid ON rostergroups(username, jid);\
    """

    execute """
    CREATE TABLE roster_version (\
    username varchar(250) PRIMARY KEY, \
    version text NOT NULL\
    ) ;\
    """

    execute "CREATE SEQUENCE offline_id_seq;"
    execute """
    CREATE TABLE offline_message(\
    id BIGINT NOT NULL DEFAULT nextval('offline_id_seq') PRIMARY KEY, \
    timestamp BIGINT NOT NULL, \
    expire BIGINT , \
    server varchar(250) NOT NULL, \
    username varchar(250) NOT NULL, \
    from_jid varchar(250) NOT NULL, \
    packet bytea NOT NULL\
    );\
    """
    execute """
    CREATE INDEX i_offline_message ON\
     offline_message(server, username, id);\
    """

    execute """
    CREATE TABLE privacy_default_list (\
    username varchar(250) PRIMARY KEY, \
    name varchar(250) NOT NULL\
    ) ;\
    """

    execute "CREATE SEQUENCE privacy_list_id_seq;"
    execute """
    CREATE TABLE privacy_list (\
    username varchar(250) NOT NULL, \
    name varchar(250) NOT NULL, \
    id BIGINT NOT NULL DEFAULT nextval('privacy_list_id_seq') UNIQUE, \
    created_at timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP, \
    PRIMARY KEY (username, name)\
    ) ;\
    """

    execute """
    CREATE TABLE privacy_list_data (\
    id bigint, \
    t character(1) NOT NULL, \
    value text NOT NULL, \
    action character(1) NOT NULL, \
    ord bigint NOT NULL, \
    match_all boolean NOT NULL, \
    match_iq boolean NOT NULL, \
    match_message boolean NOT NULL, \
    match_presence_in boolean NOT NULL, \
    match_presence_out boolean NOT NULL, \
    PRIMARY KEY (id, ord)\
    ) ;\
    """

    execute """
    CREATE TABLE private_storage (\
    username varchar(250) NOT NULL, \
    namespace varchar(250) NOT NULL, \
    data text NOT NULL, \
    created_at timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP\
    ) ;\
    """
    execute """
    CREATE INDEX i_private_storage_username ON\
     private_storage(username);\
    """
    execute """
    CREATE UNIQUE INDEX i_private_storage_username_namespace ON\
     private_storage(username, namespace);\
    """

    execute "CREATE TYPE direction AS ENUM ('I', 'O');"
    execute """
    CREATE TABLE mam_message(\
    id BIGINT NOT NULL, \
    user_id INT NOT NULL, \
    from_jid varchar(250) NOT NULL, \
    remote_bare_jid varchar(250) NOT NULL, \
    remote_resource varchar(250) NOT NULL, \
    direction direction NOT NULL, \
    message bytea NOT NULL, \
    PRIMARY KEY (user_id, id) \
    );\
    """

    execute "CREATE TYPE behaviour AS ENUM ('A', 'N', 'R');"
    execute """
    CREATE TABLE mam_config(\
    user_id INT NOT NULL, \
    remote_jid varchar(250) NOT NULL, \
    behaviour behaviour NOT NULL, \
    PRIMARY KEY (user_id, remote_jid)\
    );\
    """

    execute "CREATE SEQUENCE mam_server_user_id_seq;"
    execute """
    CREATE TABLE mam_server_user(\
    id INT NOT NULL DEFAULT nextval('mam_server_user_id_seq'), \
    server varchar(250) NOT NULL, \
    user_name varchar(250) NOT NULL, \
    PRIMARY KEY(id) , \
    CONSTRAINT uc_mam_server_user_name UNIQUE (server, user_name)\
    );\
    """

    execute """
    CREATE TABLE mam_muc_message(\
    id BIGINT NOT NULL, \
    room_id INT NOT NULL, \
    nick_name varchar(250) NOT NULL, \
    message bytea NOT NULL, \
    PRIMARY KEY (room_id, id)\
    );\
    """
  end

  def down do
    drop table(:users)
    drop table(:last)
    drop table(:rosterusers)
    drop table(:rostergroups)
    drop table(:roster_version)
    drop table(:offline_message)
    drop table(:privacy_default_list)
    drop table(:privacy_list_data)
    drop table(:privacy_list)
    drop table(:private_storage)
    drop table(:mam_message)
    drop table(:mam_config)
    drop table(:mam_server_user)
    drop table(:mam_muc_message)
  end
end
