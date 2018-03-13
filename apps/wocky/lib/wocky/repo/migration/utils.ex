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
  def remove_notify(table, action_atom) do
    action = Atom.to_string(action_atom)
    execute "DROP TRIGGER IF EXISTS #{name(table, action)} ON #{table}"
    execute "DROP FUNCTION IF EXISTS #{name(table, action)}()"
  end

  defp name(table, action), do: "notify_#{table}_#{action}"
end
