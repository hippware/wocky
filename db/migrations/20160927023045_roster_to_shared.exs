defmodule Schemata.RosterToSharedMigration do
  use Schemata.Migration, [
    authored_at: "2016-09-27T02:30:45Z",
    description: "<roster-to-shared-keyspace>"
  ]

  def up do
    drop :view, named: :roster_version, in: :wocky_db.local_keyspace
    drop :table, named: :roster, in: :wocky_db.local_keyspace
    create_table :roster, in: :wocky_db.shared_keyspace,
      columns: [
        user:         :text,     # user id (userpart of jid)
        server:       :text,     # user server (domainpart of jid)
        contact_jid:  :text,     # bare jid for contact
        active:       :boolean,  # true if the roster item is not deleted
        nick:         :text,     # display name for contact chosen by the user
        groups:       {:set, :text}, # list of groups the contact belongs to
        ask:          :text,     # status if the item is pending approval
        subscription: :text,     # subscription state of the roster item
        version:      :timestamp # timestamp indicating when the roster item
                                 # was last updated
      ],
      primary_key: [:user, :contact_jid]

    create_view :roster_version,
      from: :roster, in: :wocky_db.shared_keyspace,
      columns: :all,
      primary_key: [:user, :version, :contact_jid],
      order_by: [version: :asc]

    alter_table :bot, in: :wocky_db.local_keyspace, drop: :owner_roster
    alter_table :bot, in: :wocky_db.local_keyspace, drop: :owner_roster_ver
  end

  def down do
    drop :view, named: :roster_version, in: :wocky_db.shared_keyspace
    drop :table, named: :roster, in: :wocky_db.shared_keyspace
    create_table :roster, in: :wocky_db.local_keyspace,
      columns: [
        user:         :text,     # user id (userpart of jid)
        server:       :text,     # user server (domainpart of jid)
        contact_jid:  :text,     # bare jid for contact
        active:       :boolean,  # true if the roster item is not deleted
        nick:         :text,     # display name for contact chosen by the user
        groups:       {:set, :text}, # list of groups the contact belongs to
        ask:          :text,     # status if the item is pending approval
        subscription: :text,     # subscription state of the roster item
        version:      :timestamp # timestamp indicating when the roster item
                                 # was last updated
      ],
      primary_key: [:user, :contact_jid]
    create_view :roster_version,
      from: :roster, in: :wocky_db.local_keyspace,
      columns: :all,
      primary_key: [:user, :version, :contact_jid],
      order_by: [version: :asc]

    alter_table :bot, in: :wocky_db.local_keyspace,
      add: :owner_roster, type: {:set, :text}
    alter_table :bot, in: :wocky_db.local_keyspace,
      add: :owner_roster_ver, type: :text
  end
end
