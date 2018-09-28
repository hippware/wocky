defmodule Wocky.Repo.Migration.Utils do
  @moduledoc """
  This module provides utilities for db migration actions
  """

  use Wocky.Repo.Migration

  alias WockyDBWatcher.Watcher

  @type override :: {binary, binary}

  # Add function/trigger for DB notifications. @see WockyDBWatcher.Watcher
  # To override the encoding of particular fields, provide an override with
  # $ITEM$ as the item to be overridden. For example, to override the encoding
  # of the "title" field, use:
  # {"title", "my_encoding_function($ITEM$)"}
  def update_notify(table, actions, overrides \\ [])

  def update_notify(table, actions, overrides) when is_list(actions) do
    Enum.each(actions, &update_notify(table, &1, overrides))
  end

  def update_notify(table, action_atom, overrides) do
    action = Atom.to_string(action_atom)

    execute """
    CREATE OR REPLACE FUNCTION notify_#{table}_#{action}()
    RETURNS trigger AS $$
    DECLARE
      event_id bigint;
    BEGIN
      INSERT INTO watcher_events (payload, created_at) VALUES (
        json_build_object(
          'table', TG_TABLE_NAME
          ,'action', '#{action}'
          #{maybe_old(action, ",'old', #{wrap_overrides("OLD", overrides)}")}
          #{maybe_new(action, ",'new', #{wrap_overrides("NEW", overrides)}")}
        )::text,
        now()
      )
      RETURNING id INTO event_id;
      PERFORM pg_notify(
        'wocky_db_watcher_notify',
        json_build_object(
          'table', TG_TABLE_NAME,
          'action', '#{action}',
          'id', event_id
        )::text
      );
      RETURN NULL;
    END;
    $$ LANGUAGE plpgsql;
    """

    execute "DROP TRIGGER IF EXISTS #{name(table, action)} ON #{table}"
    add_notify_trigger(table, action)
  end

  defp wrap_overrides(object, overrides) do
    wrap_overrides("to_jsonb(#{object})", object, overrides)
  end

  defp wrap_overrides(base, _object, []), do: base

  defp wrap_overrides(base, object, [{field, action} | rest]) do
    mapped_action =
      String.replace(action, "$ITEM$", "#{object}.#{field}", global: true)

    # We have to call COALESCE here because jsonb_set is, inexplicably, STRICT.
    # See
    # https://www.postgresql.org/message-id/flat/
    # 37E2F9B3-B65B-4AF2-B2E9-436ADE37D670%40gida.in#
    # 37E2F9B3-B65B-4AF2-B2E9-436ADE37D670@gida.in
    new = """
    jsonb_set(#{base}, '{#{field}}',
              COALESCE(to_jsonb(#{mapped_action}), 'null'))
    """

    wrap_overrides(new, object, rest)
  end

  defp maybe_old("insert", _), do: ""
  defp maybe_old(_, str), do: str

  defp maybe_new("delete", _), do: ""
  defp maybe_new(_, str), do: str

  # NOTE: Triggers are hard-coded here to fire AFTER the triggering transaction
  # is comitted. We don't ever want to fire them BEFORE because that delays
  # the transaction being comitted until the function returns, which introduces
  # a pointless delay. Why is it pointless? Because the function will put the
  # message into the SQS queue, and then complete the transaction. By the time
  # the message is processed by the app, the transaction has still finished,
  # gaining us nothing.
  defp add_notify_trigger(table, action) do
    execute """
    CREATE TRIGGER #{name(table, action)}
    AFTER #{String.upcase(action)}
    ON #{table}
    FOR EACH ROW
    EXECUTE PROCEDURE #{name(table, action)}();
    """
  end

  ### Remove function/trigger
  @spec remove_notify(binary, Watcher.action()) :: term
  def remove_notify(table, actions) when is_list(actions),
    do: Enum.each(actions, &remove_notify(table, &1))

  def remove_notify(table, action_atom) do
    action = Atom.to_string(action_atom)
    execute "DROP TRIGGER IF EXISTS #{name(table, action)} ON #{table}"
    execute "DROP FUNCTION IF EXISTS #{name(table, action)}()"
  end

  defp name(table, action), do: "notify_#{table}_#{action}"

  # Add a trigger to mark home stream items as deleted when a linked
  # object is deleted
  def add_hs_delete_trigger_function(table, field) do
    execute """
    CREATE OR REPLACE FUNCTION delete_from_#{table}_trigger()
    RETURNS trigger AS $$
    BEGIN
      #{set_hs_item_deleted()}
      WHERE
        #{field} = OLD.id
        #{maybe_extra_condition(table)};
      RETURN OLD;
    END;
    $$ LANGUAGE plpgsql;
    """
  end

  def add_hs_delete_trigger(table) do
    execute """
    CREATE TRIGGER delete_from_#{table}_trigger BEFORE
      DELETE ON #{table}
      FOR EACH ROW
      EXECUTE PROCEDURE delete_from_#{table}_trigger();
    """
  end

  defp maybe_extra_condition("bot_items") do
    "AND reference_bot_id = OLD.bot_id"
  end

  defp maybe_extra_condition(_), do: ""

  def set_hs_item_deleted do
    """
      UPDATE home_stream_items SET
        class = 'deleted',
        stanza = '',
        from_jid = '',
        reference_user_id = null,
        reference_bot_id = null,
        reference_bot_item_id = null,
        updated_at = now()
    """
  end

  def drop_hs_delete_function_and_trigger(table) do
    execute """
    DROP TRIGGER delete_from_#{table}_trigger ON #{table}
    """

    execute """
    DROP FUNCTION delete_from_#{table}_trigger()
    """
  end

  def recreate_bot_private_trigger_function do
    execute """
    CREATE OR REPLACE FUNCTION update_bot_private_trigger()
    RETURNS trigger AS $$
    BEGIN
      IF NEW.public = false AND OLD.public = true THEN
        DELETE FROM bot_subscriptions WHERE bot_id = NEW.id
          AND NOT is_visible(user_id, NEW);
        #{set_hs_item_deleted()}
        WHERE
          reference_bot_id = NEW.id
          AND NOT is_visible(user_id, NEW);
      END IF;
      RETURN NEW;
    END;
    $$ LANGUAGE plpgsql;
    """
  end

  # NOTE `add_notify` is deprecated. Use `update_notify/2` above.
  # This is only kept around to allow old migrations to continue to work
  @spec add_notify(binary, Watcher.action() | [Watcher.action()], [override]) ::
          term
  def add_notify(table, actions, overrides \\ [])

  def add_notify(table, actions, overrides) when is_list(actions) do
    Enum.each(actions, &add_notify(table, &1, overrides))
  end

  def add_notify(table, action_atom, overrides) do
    action = Atom.to_string(action_atom)

    execute """
    CREATE FUNCTION notify_#{table}_#{action}()
    RETURNS trigger AS $$
    BEGIN
      PERFORM pg_notify(
        'wocky_db_watcher_notify',
        json_build_object(
          'table', TG_TABLE_NAME
          ,'action', '#{action}'
          #{maybe_old(action, ",'old', #{wrap_overrides("OLD", overrides)}")}
          #{maybe_new(action, ",'new', #{wrap_overrides("NEW", overrides)}")}
        )::text
      );
      RETURN NULL;
    END;
    $$ LANGUAGE plpgsql;
    """

    add_notify_trigger(table, action)
  end
end
