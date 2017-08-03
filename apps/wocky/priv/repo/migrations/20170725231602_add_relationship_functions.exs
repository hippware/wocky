defmodule Wocky.Repo.Migrations.AddRelationshipFunctions do
  use Ecto.Migration

  def up do
    execute """
    CREATE FUNCTION is_unblocked_friend(usr1 uuid, usr2 uuid) \
    RETURNS boolean AS $$ \
    DECLARE \
      count integer; \
    BEGIN \
      SELECT count(*) INTO count FROM roster_items \
        WHERE ((user_id = usr1 AND contact_id = usr2) \
               OR (user_id = usr2 AND contact_id = usr1)) \
              AND '__blocked__' != ALL(groups) \
              AND subscription = 'both' \
              AND ask = 'none'; \
      RETURN count = 2; \
    END; \
    $$ LANGUAGE plpgsql;\

    """

    execute """
    CREATE FUNCTION is_unblocked_follower(usr1 uuid, usr2 uuid) \
    RETURNS boolean AS $$ \
    DECLARE \
      count integer; \
    BEGIN \
      SELECT count(*) INTO count FROM roster_items \
        WHERE (((user_id = usr1 AND contact_id = usr2) \
                AND (subscription = 'to' OR subscription = 'both')) \
               OR ((user_id = usr2 AND contact_id = usr1) \
                AND (subscription = 'from' OR subscription = 'both'))) \
              AND '__blocked__' != ALL(groups) \
              AND ask = 'none'; \
      RETURN count = 2; \
    END; \
    $$ LANGUAGE plpgsql;\

    """

    create_bot_link_fun("is_shared", "bot_shares")

    create_bot_link_fun("is_subscribed", "bot_subscriptions")

    execute """
    CREATE FUNCTION is_visible(usr uuid, bot bots) \
    RETURNS boolean AS $$ \
    BEGIN \
      RETURN
        bot.user_id = usr \
        OR bot.public \
        OR is_shared(usr, bot.id); \
    END; \
    $$ LANGUAGE plpgsql;\
    """

    execute """
    CREATE FUNCTION is_searchable(usr uuid, bot bots) \
    RETURNS boolean AS $$ \
    BEGIN \
      RETURN
        bot.user_id = usr \
        OR is_subscribed(usr, bot.id) \
        OR (bot.public = TRUE AND is_unblocked_follower(usr, bot.user_id)) \
        OR (is_unblocked_friend(usr, bot.user_id) AND is_shared(usr, bot.id)); \
    END; \
    $$ LANGUAGE plpgsql;\
    """

  end

  def down do
    execute """
    DROP FUNCTION is_unblocked_friend(uuid, uuid)
    DROP FUNCTION is_unblocked_follower(uuid, uuid)
    DROP FUNCTION is_shared(uuid, uuid)
    DROP FUNCTION is_subscribed(uuid, uuid)
    DROP FUNCTION is_searchable(uuid, uuid)
    """
  end

  defp create_bot_link_fun(fun, table) do
    execute """
    CREATE FUNCTION #{fun}(usr uuid, bot uuid) \
    RETURNS boolean AS $$ \
    DECLARE \
      count integer; \
    BEGIN \
      SELECT count(*) INTO count FROM #{table} \
        WHERE (bot_id = bot AND user_id = usr); \
      RETURN count != 0; \
    END; \
    $$ LANGUAGE plpgsql;\

    """
  end
end
