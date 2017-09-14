defmodule Wocky.Repo.Migrations.UpdateRelationshipFunsForBlocking do
  use Ecto.Migration

  def up do
    execute "DROP FUNCTION is_unblocked_friend(usr1 uuid, usr2 uuid)"

    execute """
    CREATE FUNCTION is_friend(usr1 uuid, usr2 uuid) \
    RETURNS boolean AS $$ \
    DECLARE \
      count integer; \
    BEGIN \
      SELECT count(*) INTO count FROM roster_items \
        WHERE user_id = usr2 \
              AND contact_id = usr1 \
              AND subscription = 'both' \
              AND ask = 'none'; \
      RETURN count = 1; \
    END; \
    $$ LANGUAGE plpgsql; \

    """

    execute "DROP FUNCTION is_unblocked_follower(usr1 uuid, usr2 uuid)"

    execute """
    CREATE FUNCTION is_follower(usr1 uuid, usr2 uuid) \
    RETURNS boolean AS $$ \
    DECLARE \
      count integer; \
    BEGIN \
      SELECT count(*) INTO count FROM roster_items \
        WHERE user_id = usr1 \
              AND contact_id = usr2 \
              AND (subscription = 'to' OR subscription = 'both') \
              AND ask = 'none'; \
      RETURN count = 1; \
    END; \
    $$ LANGUAGE plpgsql; \

    """

    execute "drop FUNCTION is_searchable(usr uuid, bot bots)"

    execute """
    CREATE FUNCTION is_searchable(usr uuid, bot bots) \
    RETURNS boolean AS $$ \
    BEGIN \
      RETURN
        bot.user_id = usr \
        OR is_subscribed(usr, bot.id) \
        OR (bot.public = TRUE AND is_follower(usr, bot.user_id)) \
        OR (is_friend(usr, bot.user_id) AND is_shared(usr, bot.id)); \
    END; \
    $$ LANGUAGE plpgsql;\
    """
  end

end
