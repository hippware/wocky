defmodule Wocky.Repo.Migrations.AddLockToBotCalcs do
  use Ecto.Migration

  # This adds the `PERFORM...FOR UPDATE` line here. We need this because if two
  # users subscribe at the same time, this function can end up running in
  # parallel. The bot is only locked when `UPDATE` is called, so one can update
  # after the other, but the SELECTs are potentially run against the
  # pre-transaction data, so both users will only see themselves as subscribers
  # and set the count to 1, even though by the end it should be two.
  # By adding the lock on the bot at the front, we ensure that the first will
  # have completed and comitted before the SELECTs are run by the second.
  def up do
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
      PERFORM * FROM bots  WHERE id = updated_bot_id FOR UPDATE;
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
  end
end
