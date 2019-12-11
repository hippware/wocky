defmodule Wocky.Repo.Migrations.ConsolidateContacts do
  use Wocky.Repo.Migration

  alias Wocky.Contacts.Relationship
  alias Wocky.Contacts.Relationship.ContactStateEnum
  alias Wocky.Repo

  def up do
    ContactStateEnum.create_type()

    alter table("roster_items") do
      add :state, ContactStateEnum.type()
    end

    flush()

    Repo.update_all(Relationship, set: [state: :friend])

    alter table("roster_items") do
      modify :state, ContactStateEnum.type(), null: false
    end

    flush()

    migrate_invitations()
    migrate_blocks()
    update_triggers()
  end

  defmodule OldInvitation do
    @moduledoc false

    use Wocky.Repo.Schema

    schema "user_invitations" do
      field :user_id, :binary_id
      field :invitee_id, :binary_id

      field :share_type, Relationship.LocationShareTypeEnum,
        null: false,
        default: :disabled

      field :created_at, :utc_datetime_usec
      field :updated_at, :utc_datetime_usec
    end
  end

  defp migrate_invitations do
    invitations =
      OldInvitation
      |> Repo.all()
      |> Enum.map(fn invitation ->
        %{
          user_id: invitation.user_id,
          contact_id: invitation.invitee_id,
          created_at: invitation.created_at,
          updated_at: invitation.updated_at,
          share_type: invitation.share_type,
          state: :invited
        }
      end)

    Repo.insert_all(
      Relationship,
      invitations,
      on_conflict: :nothing,
      conflict_target: [:user_id, :contact_id]
    )
  end

  defmodule OldBlock do
    @moduledoc false

    use Wocky.Repo.Schema

    @primary_key false
    schema "blocks" do
      field :blocker_id, :binary_id
      field :blockee_id, :binary_id
      field :created_at, :utc_datetime_usec
      field :updated_at, :utc_datetime_usec
    end
  end

  defp migrate_blocks do
    blocks =
      OldBlock
      |> Repo.all()
      |> Enum.map(fn block ->
        %{
          user_id: block.blocker_id,
          contact_id: block.blockee_id,
          created_at: block.created_at,
          updated_at: block.updated_at,
          share_type: :disabled,
          state: :blocked
        }
      end)

    Repo.insert_all(
      Relationship,
      blocks,
      on_conflict: {:replace, [:state, :share_type, :updated_at]},
      conflict_target: [:user_id, :contact_id]
    )
  end

  defp update_triggers do
    execute """
    CREATE FUNCTION cleanup_relationships(UUID, UUID)
    RETURNS void AS $$
    BEGIN
      DELETE FROM bot_subscriptions S USING bots B
      WHERE S.bot_id = B.id
        AND B.user_id = $1 AND S.user_id = $2;

      DELETE FROM bot_invitations
      WHERE user_id = $1 AND invitee_id = $2;
    END;
    $$ LANGUAGE plpgsql
    """

    execute """
    CREATE FUNCTION post_block_cleanup(UUID, UUID)
    RETURNS void AS $$
    BEGIN
      PERFORM cleanup_relationships($1, $2);

      DELETE FROM bot_items I USING bots B
      WHERE I.bot_id = B.id
        AND ((B.user_id = $1 AND I.user_id = $2)
          OR (B.user_id = $2 AND I.user_id = $1));

      DELETE FROM notifications
      WHERE (user_id = $1 AND other_user_id = $2)
        OR  (user_id = $2 AND other_user_id = $1);

      DELETE FROM notifications N USING bots B
        WHERE N.bot_id = B.id
          AND ((B.user_id = $1 AND N.user_id = $2)
            OR (B.user_id = $2 AND N.user_id = $1));
    END;
    $$ LANGUAGE plpgsql
    """

    # Unfriending
    execute "DROP TRIGGER roster_item_delete_trigger ON roster_items"
    execute "DROP FUNCTION roster_item_delete_trigger"

    execute """
    CREATE FUNCTION roster_item_delete_trigger()
    RETURNS trigger AS $$
    BEGIN
      IF OLD.state = 'friend' THEN
        PERFORM cleanup_relationships(OLD.user_id, OLD.contact_id);
      END IF;
      RETURN NULL;
    END;
    $$ LANGUAGE plpgsql
    """

    execute """
    CREATE TRIGGER roster_item_delete_trigger
    AFTER DELETE ON roster_items
    FOR EACH ROW EXECUTE PROCEDURE roster_item_delete_trigger();
    """

    # Blocking
    execute "DROP TRIGGER block_insert_trigger ON blocks"
    execute "DROP FUNCTION block_insert_trigger"

    execute """
    CREATE FUNCTION roster_item_block_trigger()
    RETURNS trigger AS $$
    BEGIN
      IF NEW.state = 'blocked' THEN
        PERFORM post_block_cleanup(NEW.user_id, NEW.contact_id);
      END IF;
      RETURN NULL;
    END;
    $$ LANGUAGE plpgsql
    """

    execute """
    CREATE TRIGGER roster_item_block_trigger
    AFTER INSERT OR UPDATE OF state ON roster_items
    FOR EACH ROW EXECUTE PROCEDURE roster_item_block_trigger();
    """

    execute "DROP FUNCTION remove_all_relationships(UUID, UUID)"
    execute "DROP FUNCTION block_user(UUID, UUID)"
  end
end
