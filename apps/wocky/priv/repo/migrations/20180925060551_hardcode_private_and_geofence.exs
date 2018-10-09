defmodule Wocky.Repo.Migrations.HardcodePrivateAndGeofence do
  use Wocky.Repo.Migration

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
  end
end
