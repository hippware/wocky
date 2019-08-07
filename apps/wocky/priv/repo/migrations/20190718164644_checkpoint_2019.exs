defmodule Wocky.Repo.Migrations.Checkpoint2019 do
  use Wocky.Repo.Migration

  import DawdleDB.Migration

  @bot_overrides [{"location", "ST_AsGeoJSON($ITEM$, 20, 2)"}]

  def up do
    execute "CREATE EXTENSION IF NOT EXISTS unaccent;"
    execute "CREATE EXTENSION IF NOT EXISTS postgis;"
    execute "CREATE EXTENSION IF NOT EXISTS postgis_topology;"
    execute ~s|CREATE EXTENSION IF NOT EXISTS "uuid-ossp";|

    execute """
    CREATE FUNCTION max(uuid, uuid) RETURNS uuid
        LANGUAGE plpgsql
        AS $_$
    BEGIN
        IF $2 IS NULL OR $1 > $2 THEN
            RETURN $1;
        END IF;

        RETURN $2;
    END;
    $_$;
    """

    execute """
    CREATE FUNCTION min(uuid, uuid) RETURNS uuid
        LANGUAGE plpgsql
        AS $_$
    BEGIN
        IF $2 IS NULL OR $1 > $2 THEN
            RETURN $2;
        END IF;

        RETURN $1;
    END;
    $_$;
    """

    execute """
    CREATE TABLE watcher_events (
        id BIGSERIAL PRIMARY KEY,
        payload text NOT NULL,
        created_at timestamp with time zone NOT NULL
    );
    """

    execute """
    CREATE TABLE users (
        id uuid PRIMARY KEY,
        external_id character varying(255),
        handle character varying(255),
        image_url character varying(255),
        phone_number character varying(255),
        email character varying(255),
        password text,
        pass_details text,
        created_at timestamp with time zone NOT NULL,
        updated_at timestamp with time zone NOT NULL,
        tagline text NOT NULL DEFAULT ''::text,
        roles character varying(255)[] NOT NULL DEFAULT ARRAY[]::character varying[],
        provider character varying(255) DEFAULT 'firebase'::character varying,
        welcome_sent boolean NOT NULL DEFAULT false,
        smss_sent integer NOT NULL DEFAULT 0,
        bot_created boolean NOT NULL DEFAULT false,
        client_data text,
        name character varying(255) NOT NULL DEFAULT ''::character varying,
        transient boolean NOT NULL DEFAULT false
    );
    """

    execute """
    CREATE FUNCTION users_name_fts(name text, handle text) RETURNS tsvector
        LANGUAGE sql IMMUTABLE
        AS $$
    SELECT to_tsvector('simple', unaccent(name)) ||
           to_tsvector('simple', unaccent(handle));
    $$;
    """

    execute """
    CREATE UNIQUE INDEX users_external_id_index ON users USING btree (external_id);
    """

    execute """
    CREATE UNIQUE INDEX users_lower_handle_index ON users USING btree (lower((handle)::text));
    """

    execute """
    CREATE INDEX users_name_fts ON users USING gin (users_name_fts((name)::text, (handle)::text));
    """

    update_notify("users", [:update])

    execute """
    CREATE TABLE blocks (
        blocker_id uuid REFERENCES users(id) ON DELETE CASCADE,
        blockee_id uuid REFERENCES users(id) ON DELETE CASCADE,
        created_at timestamp with time zone NOT NULL,
        updated_at timestamp with time zone NOT NULL,
        CONSTRAINT blocks_pkey PRIMARY KEY (blocker_id, blockee_id)
    );
    """

    execute """
    CREATE INDEX blocks_blockee_id_index ON blocks USING btree (blockee_id);
    """

    execute """
    CREATE INDEX blocks_blocker_id_index ON blocks USING btree (blocker_id);
    """

    update_notify("blocks", [:insert])

    execute """
    CREATE TABLE bots (
        id uuid PRIMARY KEY,
        pending boolean NOT NULL DEFAULT false,
        title character varying(255) NOT NULL DEFAULT ''::character varying,
        shortname character varying(255),
        user_id uuid NOT NULL REFERENCES users(id) ON DELETE CASCADE,
        image_url character varying(255),
        type character varying(255) NOT NULL DEFAULT ''::character varying,
        address character varying(255) NOT NULL DEFAULT ''::character varying,
        radius double precision NOT NULL DEFAULT 100.0,
        tags character varying(255)[] NOT NULL DEFAULT ARRAY[]::character varying[],
        created_at timestamp with time zone NOT NULL,
        updated_at timestamp with time zone NOT NULL,
        description text NOT NULL DEFAULT ''::text,
        location geography(Point,4326),
        address_data text,
        icon text NOT NULL DEFAULT ''::text
    );
    """

    execute """
    CREATE INDEX bot_location_gix ON bots USING gist (location);
    """

    execute """
    CREATE UNIQUE INDEX bots_shortname_index ON bots USING btree (shortname);
    """

    update_notify("bots", [:insert, :update, :delete], @bot_overrides)

    execute """
    CREATE TABLE bot_invitations (
        id BIGSERIAL PRIMARY KEY,
        user_id uuid NOT NULL REFERENCES users(id) ON DELETE CASCADE,
        invitee_id uuid NOT NULL REFERENCES users(id) ON DELETE CASCADE,
        bot_id uuid NOT NULL REFERENCES bots(id) ON DELETE CASCADE,
        accepted boolean,
        created_at timestamp with time zone NOT NULL,
        updated_at timestamp with time zone NOT NULL
    );
    """

    execute """
    CREATE UNIQUE INDEX bot_invitations_user_id_bot_id_invitee_id_index ON bot_invitations USING btree (user_id, bot_id, invitee_id);
    """

    update_notify("bot_invitations", [:insert, :update])

    execute """
    CREATE TABLE bot_items (
        bot_id uuid NOT NULL REFERENCES bots(id) ON DELETE CASCADE,
        created_at timestamp with time zone NOT NULL,
        updated_at timestamp with time zone NOT NULL,
        user_id uuid REFERENCES users(id) ON DELETE CASCADE,
        id uuid PRIMARY KEY,
        content text,
        image_url character varying(255)
    );
    """

    execute """
    CREATE INDEX bot_items_bot_id_index ON bot_items USING btree (bot_id);
    """

    update_notify("bot_items", [:insert, :delete])

    execute """
    CREATE TABLE bot_subscriptions (
        user_id uuid REFERENCES users(id) ON DELETE CASCADE,
        bot_id uuid REFERENCES bots(id) ON DELETE CASCADE,
        created_at timestamp with time zone NOT NULL,
        updated_at timestamp with time zone NOT NULL,
        visitor boolean DEFAULT false,
        visited_at timestamp with time zone,
        departed_at timestamp with time zone,
        CONSTRAINT bot_subscriptions_pkey PRIMARY KEY (user_id, bot_id)
    );
    """

    update_notify("bot_subscriptions", [:insert, :update, :delete])

    execute """
    CREATE VIEW bot_activity AS  SELECT subs.bot_id,
        max(subs.visited_at) AS visited_at
       FROM bot_subscriptions subs
      WHERE (EXISTS ( SELECT 1
               FROM bot_subscriptions
              WHERE bot_subscriptions.bot_id = subs.bot_id AND bot_subscriptions.visitor))
      GROUP BY subs.bot_id;
    """

    execute """
    CREATE TABLE client_versions (
        user_id uuid REFERENCES users(id) ON DELETE CASCADE,
        device character varying(255),
        version character varying(255) NOT NULL,
        attributes character varying(255)[] NOT NULL DEFAULT ARRAY[]::character varying[],
        created_at timestamp with time zone NOT NULL,
        updated_at timestamp with time zone NOT NULL,
        CONSTRAINT client_versions_pkey PRIMARY KEY (user_id, device)
    );
    """

    execute """
    CREATE TABLE messages (
        id BIGSERIAL PRIMARY KEY,
        sender_id uuid NOT NULL REFERENCES users(id) ON DELETE CASCADE,
        recipient_id uuid NOT NULL REFERENCES users(id) ON DELETE CASCADE,
        migration_id bigint,
        created_at timestamp with time zone NOT NULL,
        content text,
        image_url character varying(255),
        client_data text,
        read boolean NOT NULL DEFAULT false,
        updated_at timestamp(0) without time zone
    );
    """

    execute """
    CREATE INDEX messages_recipient_id_index ON messages USING btree (recipient_id);
    """

    execute """
    CREATE INDEX messages_sender_id_index ON messages USING btree (sender_id);
    """

    update_notify("messages", [:insert])

    execute """
    CREATE VIEW conversations AS ( SELECT DISTINCT ON ((min(messages.sender_id, messages.recipient_id)), (max(messages.sender_id, messages.recipient_id))) messages.id,
        messages.sender_id AS user_id,
        messages.recipient_id AS other_user_id,
        messages.content,
        messages.image_url,
        messages.client_data,
        messages.read,
        messages.created_at,
        messages.updated_at,
        'outgoing'::text AS direction
       FROM messages
      GROUP BY messages.created_at, messages.id, messages.sender_id, messages.recipient_id
      ORDER BY (min(messages.sender_id, messages.recipient_id)), (max(messages.sender_id, messages.recipient_id)), messages.created_at DESC)
    UNION ALL
    ( SELECT DISTINCT ON ((min(messages.sender_id, messages.recipient_id)), (max(messages.sender_id, messages.recipient_id))) messages.id,
        messages.recipient_id AS user_id,
        messages.sender_id AS other_user_id,
        messages.content,
        messages.image_url,
        messages.client_data,
        messages.read,
        messages.created_at,
        messages.updated_at,
        'incoming'::text AS direction
       FROM messages
      GROUP BY messages.created_at, messages.id, messages.sender_id, messages.recipient_id
      ORDER BY (min(messages.sender_id, messages.recipient_id)), (max(messages.sender_id, messages.recipient_id)), messages.created_at DESC);
    """

    execute """
    CREATE TABLE notifications (
        id BIGSERIAL PRIMARY KEY,
        user_id uuid NOT NULL REFERENCES users(id) ON DELETE CASCADE,
        type character varying(255) NOT NULL,
        other_user_id uuid NOT NULL REFERENCES users(id) ON DELETE CASCADE,
        bot_id uuid REFERENCES bots(id) ON DELETE CASCADE,
        bot_item_id uuid REFERENCES bot_items(id) ON DELETE CASCADE,
        bot_invitation_id bigint REFERENCES bot_invitations(id) ON DELETE CASCADE,
        geofence_event character varying(255),
        bot_invitation_accepted boolean,
        created_at timestamp with time zone NOT NULL,
        updated_at timestamp with time zone NOT NULL,
        expires_at timestamp(0) without time zone
    );
    """

    # NOTE This constraint is named inconsistently in the original schema.
    # I am keeping the original naming here so that this file recreates the
    # schema as close to the original as possible.
    execute """
    ALTER TABLE notifications
        RENAME CONSTRAINT notifications_bot_invitation_id_fkey TO notifications_invitation_id_fkey;
    """

    update_notify("notifications", [:insert, :delete])

    execute """
    CREATE TABLE push_tokens (
        id uuid PRIMARY KEY,
        user_id uuid REFERENCES users(id) ON DELETE CASCADE,
        device character varying(255) NOT NULL,
        token character varying(255) NOT NULL,
        valid boolean NOT NULL DEFAULT true,
        enabled_at timestamp with time zone,
        disabled_at timestamp with time zone,
        invalidated_at timestamp with time zone,
        created_at timestamp with time zone NOT NULL,
        platform character varying(255) NOT NULL DEFAULT 'apns'::character varying,
        dev_mode boolean NOT NULL DEFAULT false
    );
    """

    execute """
    CREATE INDEX push_tokens_user_id_index ON push_tokens USING btree (user_id);
    """

    execute """
    CREATE UNIQUE INDEX push_tokens_user_id_resource_token_index ON push_tokens USING btree (user_id, device, token);
    """

    execute """
    CREATE TABLE roster_items (
        id BIGSERIAL PRIMARY KEY,
        user_id uuid REFERENCES users(id) ON DELETE CASCADE,
        contact_id uuid REFERENCES users(id) ON DELETE CASCADE,
        name character varying(255) NOT NULL,
        created_at timestamp with time zone NOT NULL,
        updated_at timestamp with time zone NOT NULL
    );
    """

    execute """
    CREATE UNIQUE INDEX roster_items_user_id_contact_id_index ON roster_items USING btree (user_id, contact_id);
    """

    update_notify("roster_items", [:insert, :update, :delete])

    execute """
    CREATE TABLE tros_metadatas (
        id uuid PRIMARY KEY,
        user_id uuid NOT NULL REFERENCES users(id) ON DELETE CASCADE,
        access text NOT NULL,
        created_at timestamp with time zone NOT NULL,
        updated_at timestamp with time zone NOT NULL,
        ready boolean DEFAULT true
    );
    """

    update_notify("tros_metadatas", [:update])

    execute """
    CREATE TABLE user_bot_events (
        id uuid PRIMARY KEY,
        user_id uuid NOT NULL REFERENCES users(id) ON DELETE CASCADE,
        bot_id uuid NOT NULL REFERENCES bots(id) ON DELETE CASCADE,
        event character varying(255) NOT NULL,
        created_at timestamp with time zone NOT NULL,
        occurred_at timestamp with time zone NOT NULL,
        device character varying(255),
        location_id character varying(255)
    );
    """

    execute """
    CREATE TABLE user_invitations (
        id BIGSERIAL PRIMARY KEY,
        user_id uuid REFERENCES users(id) ON DELETE CASCADE,
        invitee_id uuid REFERENCES users(id) ON DELETE CASCADE,
        created_at timestamp with time zone NOT NULL,
        updated_at timestamp with time zone NOT NULL
    );
    """

    execute """
    CREATE UNIQUE INDEX user_invitations_user_id_invitee_id_index ON user_invitations USING btree (user_id, invitee_id);
    """

    update_notify("user_invitations", [:insert])

    execute """
    CREATE TABLE user_invite_codes (
        id BIGSERIAL PRIMARY KEY,
        user_id uuid REFERENCES users(id) ON DELETE CASCADE,
        code character varying(255) NOT NULL,
        created_at timestamp with time zone NOT NULL
    );
    """

    execute """
    CREATE INDEX user_invite_codes_code_index ON user_invite_codes USING btree (code);
    """

    execute """
    CREATE TABLE user_location_shares (
        id BIGSERIAL PRIMARY KEY,
        user_id uuid REFERENCES users(id) ON DELETE CASCADE,
        shared_with_id uuid REFERENCES users(id) ON DELETE CASCADE,
        expires_at timestamp(0) without time zone NOT NULL,
        created_at timestamp with time zone NOT NULL,
        updated_at timestamp with time zone NOT NULL
    );
    """

    execute """
    CREATE INDEX user_location_shares_expires_at_index ON user_location_shares USING btree (expires_at);
    """

    execute """
    CREATE UNIQUE INDEX user_location_shares_user_id_shared_with_id_index ON user_location_shares USING btree (user_id, shared_with_id);
    """

    update_notify("user_location_shares", [:insert, :update, :delete])

    execute """
    CREATE FUNCTION is_follower(usr1 uuid, usr2 uuid) RETURNS boolean
        LANGUAGE plpgsql
        AS $$ DECLARE   count integer; BEGIN   SELECT count(*) INTO count FROM roster_items     WHERE user_id = usr1           AND contact_id = usr2           AND (subscription = 'to' OR subscription = 'both')           AND ask = 'none';   RETURN count = 1; END; $$;
    """

    execute """
    CREATE FUNCTION is_friend(usr1 uuid, usr2 uuid) RETURNS boolean
        LANGUAGE plpgsql
        AS $$ DECLARE   count integer; BEGIN   SELECT count(*) INTO count FROM roster_items     WHERE user_id = usr2           AND contact_id = usr1           AND subscription = 'both'           AND ask = 'none';   RETURN count = 1; END; $$;
    """

    execute """
    CREATE FUNCTION is_shared(usr uuid, bot uuid) RETURNS boolean
        LANGUAGE plpgsql
        AS $$ DECLARE   count integer; BEGIN   SELECT count(*) INTO count FROM bot_shares     WHERE (bot_id = bot AND user_id = usr);   RETURN count != 0; END; $$;
    """

    execute """
    CREATE FUNCTION is_subscribed(usr uuid, bot uuid) RETURNS boolean
        LANGUAGE plpgsql
        AS $$ DECLARE   count integer; BEGIN   SELECT count(*) INTO count FROM bot_subscriptions     WHERE (bot_id = bot AND user_id = usr);   RETURN count != 0; END; $$;
    """

    execute """
    CREATE FUNCTION is_searchable(usr uuid, bot public.bots) RETURNS boolean
        LANGUAGE plpgsql
        AS $$
    BEGIN
      RETURN
        bot.user_id = usr
        OR is_subscribed(usr, bot.id);
    END;
    $$;
    """
  end
end
