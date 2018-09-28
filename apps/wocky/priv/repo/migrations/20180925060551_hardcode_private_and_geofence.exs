defmodule Wocky.Repo.Migrations.HardcodePrivateAndGeofence do
  use Ecto.Migration

  def up do
    execute "DROP TRIGGER update_bot_private_trigger on bots"
    execute "DROP FUNCTION update_bot_private_trigger()"
    execute "DROP FUNCTION is_visible(usr uuid, bot bots)"

    execute """
    CREATE FUNCTION is_visible(usr uuid, bot bots) \
    RETURNS boolean AS $$ \
    BEGIN \
      RETURN
        bot.user_id = usr \
        OR is_shared(usr, bot.id); \
    END; \
    $$ LANGUAGE plpgsql;\
    """

    alter table(:bots) do
      remove :public
      remove :geofence
    end

    alter table(:bot_subscriptions) do
      remove :guest
    end

    # Clean out all the existing data so that we start afresh with the
    # new client version
    execute "DELETE FROM bot_subscriptions"
    execute "DELETE FROM bot_shares"
    execute "DELETE FROM bot_invitations"
  end
end
