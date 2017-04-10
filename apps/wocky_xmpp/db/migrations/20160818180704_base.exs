defmodule Schemata.BaseMigration do
  use Schemata.Migration, [
    authored_at: "2016-08-18T18:07:04Z",
    description: "Base schema"
  ]

  def up do
    create_table :user, in: :wocky_db.shared_keyspace,
      columns: [
        user:         :text,
        server:       :text,
        handle:       :text,
        password:     :text,
        avatar:       :text,
        first_name:   :text,
        last_name:    :text,
        email:        :text,
        external_id:  :text
      ],
      primary_key: [:server, :user]

    create_view :external_id_to_user, in: :wocky_db.shared_keyspace,
      from: :user,
      columns: :all,
      primary_key: [:external_id, :server, :user]

    create_table :phone_number_to_user, in: :wocky_db.shared_keyspace,
      columns: [
        user:         :text,
        server:       :text,
        phone_number: :text
      ],
      primary_key: :phone_number

    create_view :user_to_phone_number, in: :wocky_db.shared_keyspace,
      from: :phone_number_to_user,
      columns: :all,
      primary_key: [:user, :phone_number]

    create_table :handle_to_user, in: :wocky_db.shared_keyspace,
      columns: [
        user:         :text,
        server:       :text,
        handle:       :text
      ],
      primary_key: :handle

    create_table :location, in: :wocky_db.local_keyspace,
      columns: [
        user:      :text,
        server:    :text,
        resource:  :text,
        time:      :timestamp,
        lat:       :double,
        lon:       :double,
        accuracy:  :double
      ],
      primary_key: [[:user, :server], :time],
      order_by: [time: :desc]

    create_table :last_activity, in: :wocky_db.local_keyspace,
      columns: [
        user:      :text,
        server:    :text,
        timestamp: :timestamp,
        status:    :text
      ],
      primary_key: :user

    create_table :offline_msg, in: :wocky_db.local_keyspace,
      columns: [
        user:      :text,
        server:    :text,
        msg_id:    :timeuuid,
        timestamp: :timestamp,
        expire:    :timestamp,
        from_id:   :text,
        to_id:     :text,
        packet:    :text
      ],
      primary_key: [:user, :timestamp, :msg_id],
      order_by: [timestamp: :asc]

    create_table :roster, in: :wocky_db.local_keyspace,
      columns: [
        user:         :text,
        server:       :text,
        contact_jid:  :text,
        active:       :boolean,
        nick:         :text,
        groups:       {:set, :text},
        ask:          :text,
        ask_message:  :text,
        subscription: :text,
        version:      :timestamp
      ],
      primary_key: [:user, :contact_jid]

    create_view :roster_version, in: :wocky_db.local_keyspace,
      from: :roster,
      columns: :all,
      primary_key: [:user, :version, :contact_jid],
      order_by: [version: :asc]

    create_table :session, in: :wocky_db.local_keyspace,
      columns: [
        sid:          :blob,
        node:         :text,
        user:         :text,
        server:       :text,
        jid_user:     :text,
        jid_server:   :text,
        jid_resource: :blob,
        priority:     :int,
        info:         :blob
      ],
      primary_key: [:sid, :jid_user]

    create_index in: :wocky_db.local_keyspace, on: :session, keys: [:node]

    create_view :user_sessions, in: :wocky_db.local_keyspace,
      from: :session,
      columns: :all,
      primary_key: [:jid_user, :jid_resource, :sid]

    create_table :media, in: :wocky_db.local_keyspace,
      columns: [
        id:        :text,
        user:      :text,
        size:      :int,
        purpose:   :text,
        access:    :text,
        metadata:  {:map, :text, :text},
        chunks:    {:list, :timeuuid}
      ],
      primary_key: :id

    create_table :media_data, in: :wocky_db.local_keyspace,
      columns: [
        chunk_id:  :timeuuid,
        file_id:   :text,
        data:      :blob
      ],
      primary_key: :chunk_id

    create_table :message_archive, in: :wocky_db.local_keyspace,
      columns: [
        id:        :varint,
        user_jid:  :text,
        other_jid: :text,
        time:      :timeuuid,
        outgoing:  :boolean,
        message:   :blob
      ],
      primary_key: [:user_jid, :time],
      order_by: [time: :asc]

    create_view :archive_id, in: :wocky_db.local_keyspace,
      from: :message_archive,
      columns: [:id, :user_jid, :other_jid, :time],
      primary_key: [:user_jid, :id, :time]

    create_table :conversation, in: :wocky_db.local_keyspace,
      columns: [
        id:        :varint,
        user_jid:  :text,
        other_jid: :text,
        time:      :timeuuid,
        outgoing:  :boolean,
        message:   :blob
      ],
      primary_key: [:user_jid, :other_jid]

    create_table :auth_token, in: :wocky_db.local_keyspace,
      columns: [
        user:       :text,
        server:     :text,
        resource:   :text,
        auth_token: :text,
        created_at: :timestamp,
        expires_at: :timestamp
      ],
      primary_key: [:user, :server, :resource]

    create_table :privacy, in: :wocky_db.local_keyspace,
      columns: [
        user:      :text,
        server:    :text,
        default:   :text,
        lists:     {:set, :text}
      ],
      primary_key: [:user, :server]

    create_table :privacy_item, in: :wocky_db.local_keyspace,
      columns: [
        user:       :text,
        server:     :text,
        list:       :text,
        id:         :timeuuid,
        type:       :text,
        value:      :text,
        action:     :boolean,
        item_order: :int,
        match_all:          :boolean,
        match_iq:           :boolean,
        match_message:      :boolean,
        match_presence_in:  :boolean,
        match_presence_out: :boolean
      ],
      primary_key: [:user, :server, :list, :id]

    create_table :tros_request, in: :wocky_db.local_keyspace,
      columns: [
        user:      :text,
        file:      :text,
        auth:      :blob,
        method:    :text,
        size:      :int,
        metadata:  {:map, :text, :text},
        purpose:   :text,
        access:    :text
      ],
      primary_key: [:user, :file, :auth, :method]

    create_table :phone_lookup_count, in: :wocky_db.local_keyspace,
      columns: [
        user:      :text,
        server:    :text,
        date:      :timestamp,
        count:     :int
      ],
      primary_key: [:user, :server, :date]

    create_table :group_chat, in: :wocky_db.local_keyspace,
      columns: [
        id:           :timeuuid,
        owner:        :timeuuid,
        participants: {:set, :text},
        title:        :text
      ],
      primary_key: [:id]
  end
end
