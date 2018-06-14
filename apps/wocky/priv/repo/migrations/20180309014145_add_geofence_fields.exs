defmodule Wocky.Repo.Migrations.AddGeofenceFields do
  use Wocky.Repo.Migration

  import Ecto.Query

  alias Wocky.Bot
  alias Wocky.Repo

  def change do
    alter table(:bots) do
      add :geofence, :boolean, default: false

      add :guests_hash, :string,
        default: "d41d8cd98f00b204e9800998ecf8427e", # md5("")
        null: false
      add :guests_count, :integer, default: 0, null: false

      add :visitors_hash, :string,
        default: "d41d8cd98f00b204e9800998ecf8427e", # md5("")
        null: false
      add :visitors_count, :integer, default: 0, null: false
    end

    alter table(:bot_subscriptions) do
      add :guest, :boolean, default: false
      add :visitor, :boolean, default: false
    end

    flush()

    # Update the subscriber count/hash function to also handle
    # guests and visitors
    execute """
    CREATE OR REPLACE FUNCTION update_bot_subscribers(updated_bot_id uuid)
    RETURNS void AS $$
    DECLARE
      shash text;
      scount integer;
      ghash text;
      gcount integer;
      vhash text;
      vcount integer;
    BEGIN
      SELECT md5(string_agg(user_id::text, '')), COUNT(user_id)
        INTO shash, scount
        FROM bot_subscriptions
        WHERE bot_id = updated_bot_id;
      SELECT md5(string_agg(user_id::text, '')), COUNT(user_id)
        INTO ghash, gcount
        FROM bot_subscriptions
        WHERE bot_id = updated_bot_id AND guest = true;
      SELECT md5(string_agg(user_id::text, '')), COUNT(user_id)
        INTO vhash, vcount
        FROM bot_subscriptions
        WHERE bot_id = updated_bot_id AND visitor = true;
      IF shash IS NULL THEN
        shash := md5('');
      END IF;
      IF ghash IS NULL THEN
        ghash := md5('');
      END IF;
      IF vhash IS NULL THEN
        vhash := md5('');
      END IF;

      UPDATE bots SET
        subscribers_hash = shash,
        subscribers_count = scount,
        guests_hash = ghash,
        guests_count = gcount,
        visitors_hash = vhash,
        visitors_count = vcount
        WHERE id = updated_bot_id;
    END;
    $$ LANGUAGE plpgsql;
    """

    # Explicitly set all owners as subscribers/guests
    from(b in Bot, select: {b.id, b.user_id})
    |> Repo.stream
    |> Stream.each(&add_owner_subscription/1)
    |> Stream.run()
  end

  defp add_owner_subscription({bot_id, owner_id}) do
    execute """
    INSERT INTO bot_subscriptions (bot_id, user_id, created_at, updated_at)
    VALUES (#{bot_id}, #{owner_id}, now(), now())
    """
  end
end
