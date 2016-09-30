defmodule Schemata.Schemas.Wocky do
  use Schemata.Schema

  keyspace ~r/^wocky_(test_)?shared$/ do
    # Table for storing the details of a user's account
    table :user, [
      columns: [
        user:         :text, # User ID (userpart of JID)
        server:       :text, # User Server (domainpart of JID)
        handle:       :text, # User handle (as seen by other users)
        password:     :text, # Password hash
        avatar:       :text, # ID of file containing user's avatar
        first_name:   :text, # User's first name
        last_name:    :text, # User's last name
        email:        :text, # User's email address
        external_id:  :text, # The user ID received from Twitter Digits
        roster_viewers: {:set, :text} # Set of entities allowed to view
                                      # this user's roster
      ],
      primary_key: [:server, :user]
    ]

    view :external_id_to_user, [
      from: :user,
      columns: :all,
      primary_key: [:external_id, :server, :user]
    ]

    # A lookup table that maps globally unique phone number to user account id
    table :phone_number_to_user, [
      columns: [
        user:         :text,
        server:       :text,
        phone_number: :text
      ],
      primary_key: :phone_number
    ]

    view :user_to_phone_number, [
      from: :phone_number_to_user,
      columns: :all,
      primary_key: [:user, :phone_number]
    ]


    # A lookup table that maps globally unique handle to user account id
    table :handle_to_user, [
      columns: [
        user:         :text,
        server:       :text,
        handle:       :text
      ],
      primary_key: :handle
    ]

    # Table for storing a user's roster
    table :roster, [
      columns: [
        user:         :text,     # User ID (userpart of JID)
        server:       :text,     # User Server (domainpart of JID)
        contact_jid:  :text,     # Bare JID for contact
        active:       :boolean,  # True if the roster item is not deleted
        nick:         :text,     # Display name for contact chosen by the user
        groups:       {:set, :text}, # List of groups the contact belongs to
        ask:          :text,     # Status if the item is pending approval
        subscription: :text,     # Subscription state of the roster item
        version:      :timestamp # Timestamp indicating when the roster item
                                 # was last updated
      ],
      primary_key: [:user, :contact_jid]
    ]

    view :roster_version, [
      from: :roster,
      columns: :all,
      primary_key: [:user, :version, :contact_jid],
      order_by: [version: :asc]
    ]

    table :bot, [
      columns: [
        id:               :timeuuid, # Bot ID
        server:           :text,     # Bot server
        title:            :text,     # Bot title
        shortname:        :text,     # Bot shortname for URL representation
        owner:            :text,     # Bot owner
        description:      :text,     # User-supplied description
        image:            :text,     # Bot graphical image
        type:             :text,     # Bot type (freeform string from
                                     #           server's perspective)
        lat:              :double,   # Latitude
        lon:              :double,   # Longditude
        radius:           :int,      # Radius of bot circle
        visibility:       :int,      # Visibility of bot
        affiliates:       {:set, :text}, # Bot's affiliates
                                         # (required for WHITELIST visibility)
        alerts:           :int       # Whether alerts are enabled (0/1)
      ],
      primary_key: :id
    ]

    view :user_bot, [ # MV for looking up bots by owner
      from: :bot,
      columns: [:owner, :id],
      primary_key: [:owner, :id]
    ]

  end

  keyspace ~r/^wocky_((test_)?localhost|.*_tinyrobot_com)$/ do
    # Table for storing the location history of users
    table :location, [
      columns: [
        user:      :text,      # User ID (userpart of JID)
        server:    :text,      # User Server (domainpart of JID)
        resource:  :text,      # Resource that reported this location
        time:      :timestamp, # Time of location report
        lat:       :double,    # Latitude (degrees North)
        lon:       :double,    # Longditude (degrees East)
        accuracy:  :double     # Accuracy reported by device (meters)
      ],
      primary_key: [[:user, :server], :time],
      order_by: [time: :desc]
    ]

    # Table for storing details of users' last activty on the server. This is
    # updated only when a user logs out or disconnects
    table :last_activity, [
      columns: [
        user:      :text,      # User ID (userpart of JID)
        server:    :text,      # User Server (domainpart of JID)
        timestamp: :timestamp, # Timestamp of last user logoff
        status:    :text       # Text set in last user presence
                               # with type of "unavailable"
      ],
      primary_key: :user
    ]

    # Table for storing messages sent to a user while they're offline
    table :offline_msg, [
      columns: [
        user:      :text,      # User ID (userpart of JID)
        server:    :text,      # User Server (domainpart of JID)
        msg_id:    :timeuuid,  # Unique message ID
        timestamp: :timestamp, # Message timestamp
        expire:    :timestamp, # Message expiry (as timestamp)
        from_id:   :text,      # Sending user JID
        to_id:     :text,      # Receiving user JID
        packet:    :text       # Full XML of <message> element
      ],
      primary_key: [:user, :timestamp, :msg_id],
      order_by: [timestamp: :asc]
    ]

    # Table for storing transient data for active user sessions
    table :session, [
      columns: [
        sid:          :blob,     # Session ID
        node:         :text,     # Node handling the active session
        user:         :text,     # User ID (userpart of JID)
        server:       :text,     # User Server (domainpart of JID)
        jid_user:     :text,     # Provided JID userpart
        jid_server:   :text,     # Provided JID domainpart
        jid_resource: :blob,     # Provided JID resourcepart
        priority:     :int,      # Session priority
        info:         :blob      # Session info
      ],
      primary_key: [:sid, :jid_user]
    ]

    index [
      on: :session,
      keys: [:node]
    ]

    view :user_sessions, [
      from: :session,
      columns: :all,
      primary_key: [:jid_user, :jid_resource, :sid]
    ]

    # Francus file-store metadata table
    table :media, [
      columns: [
        id:        :text,      # ID of the file
        user:      :text,      # User ID of the file owner
        size:      :int,       # File size in bytes
        purpose:   :text,      # Purpose of this file
        access:    :text,      # Comma-separated list of JIDs with access to
                               # this file. The exact meaning of the field
                               # depends on the value in the purpose field.
                               # See tros_permissions.erl
        metadata:  {:map, :text, :text}, # General purpose metadata field
        chunks:    {:list, :timeuuid} # Ordered list of media_data table
                                      # chunks comprising the file
      ],
      primary_key: :id
    ]

    # Franks file-store data table
    table :media_data, [
      columns: [
        chunk_id:  :timeuuid,  # ID of chunk
        file_id:   :text,      # ID of the file
        data:      :blob       # Data in this chunk
      ],
      primary_key: :chunk_id
    ]

    table :message_archive, [
      columns: [
        id:        :varint, # IDs are 64-bit unsigned
        user_jid:  :text,
        other_jid: :text,
        time:      :timeuuid,
        outgoing:  :boolean,
        message:   :blob
      ],
      primary_key: [:user_jid, :time],
      order_by: [time: :asc]
    ]

    # Lookup message by ID
    view :archive_id, [
      from: :message_archive,
      columns: [:id, :user_jid, :other_jid, :time],
      primary_key: [:user_jid, :id, :time]
    ]

    table :conversation, [
      columns: [
        id:        :varint, # IDs are 64-bit unsigned
        user_jid:  :text,
        other_jid: :text,
        time:      :timeuuid,
        outgoing:  :boolean,
        message:   :blob
      ],
      primary_key: [:user_jid, :other_jid]
    ]

    # Tokens for authenticating individual resources
    table :auth_token, [
      columns: [
        user:       :text,      # User ID (userpart of JID)
        server:     :text,      # Server (domainpart of JID)
        resource:   :text,      # Resource (resourcepart of JID)
        auth_token: :text,      # Token
        created_at: :timestamp, # When the token was created
        expires_at: :timestamp  # When the token expires
      ],
      primary_key: [:user, :server, :resource]
    ]

    # Stores the device IDs necessary to send notifications
    table :device, [
      columns: [
        user:       :text,      # User ID (userpart of JID)
        server:     :text,      # Server (domainpart of JID)
        resource:   :text,      # Resource (resourcepart of JID)
        platform:   :text,      # Client OS platform (apple or google)
        device_id:  :text,      # Device ID
        endpoint:   :text,      # SNS application endpoint ARN
        created_at: :timestamp  # When the device was registered
      ],
      primary_key: [:user, :server, :resource]
    ]

    # mod_privacy settings for users
    table :privacy, [
      columns: [
        user:      :text,      # User ID (userpart of JID)
        server:    :text,      # Server (domainpart of JID)
        lists:     {:set, :text} # Set of configured privacy lists
      ],
      primary_key: [:user, :server]
    ]

    # mod_privacy privacy list items
    table :privacy_item, [
      columns: [
        user:       :text,     # User ID (userpart of JID)
        server:     :text,     # Server (domainpart of JID)
        list:       :text,     # List name for this item
        id:         :timeuuid, # ID of this item
        type:       :text,     # Type of this item: jid | subscription | group
        value:      :text,     # For subscriptions: none | from | to | both
                               # For JID: String representation
                               # For group: Name of the group
        action:     :boolean,  # true = allow | false = deny
        item_order: :int,      # Sequence in which to apply this item
        # Events to which to apply this item:
        match_all:          :boolean,
        match_iq:           :boolean,
        match_message:      :boolean,
        match_presence_in:  :boolean,
        match_presence_out: :boolean
      ],
      primary_key: [:user, :server, :list, :id]
    ]

    # Table of pending TROS/Francus requests
    table :tros_request, [
      columns: [
        user:      :text,      # User making the request
        file:      :text,      # File name of the request
        auth:      :blob,      # Authorization key for the request
        method:    :text,      # HTTP method for the request (get/post)
        size:      :int,       # Size of the requested file (upload only)
        metadata:  {:map, :text, :text}, # File metadata (key => value)
        # See media table for a full description of these:
        purpose:   :text,      # Purpose of the file (upload only)
        access:    :text       # Access field for the file (upload only)
      ],
      primary_key: [:user, :file, :auth, :method]
    ]

    table :phone_lookup_count, [
      columns: [
        user:      :text,      # User ID (userpart of JID)
        server:    :text,      # Server (domainpart of JID)
        date:      :timestamp, # Date of the last request
        count:     :int        # Number of requests during the day
      ],
      primary_key: [:user, :server, :date]
    ]

    table :group_chat, [
      columns: [
        id:           :timeuuid,
        owner:        :timeuuid,
        participants: {:set, :text},
        title:        :text
      ],
      primary_key: [:id]
    ]

    table :bot_subscriber, [
      columns: [
        bot:      :timeuuid, # Bot ID
        user:     :text,     # User ID
        follow:   :boolean   # Whether user is a follower
      ],
      primary_key: [:bot, :user]
    ]

    table :bot_name, [ # Table for looking up bots by shortname (URL)
      columns: [
        shortname:    :text,
        id:           :timeuuid
      ],
      primary_key: :shortname
    ]

    table :bot_item, [
      columns: [
        id:               :text,
        bot:              :timeuuid,
        published:        :timestamp,
        updated:          :timestamp,
        stanza:           :text
      ],
      primary_key: [:bot, :id]
    ]
  end
end
