defmodule Wocky.Repo.Migrations.SimplifyExploreFilter do
  use Wocky.Repo.Migration

  def up do
    execute "drop FUNCTION is_searchable(usr uuid, bot bots)"

    execute """
    CREATE FUNCTION is_searchable(usr uuid, bot bots)
    RETURNS boolean AS $$
    BEGIN
      RETURN
        bot.user_id = usr
        OR is_subscribed(usr, bot.id);
    END;
    $$ LANGUAGE plpgsql;
    """
  end
end
