defmodule Wocky.Repo.Migrations.RemoveMessageXml do
  use Wocky.Repo.Migration

  import Ecto.Query
  import SweetXml

  alias Ecto.Changeset
  alias Wocky.Repo

  defmodule Message do
    use Wocky.Repo.Schema

    schema "messages" do
      field :message, :string
      field :content, :string
      field :image_url, :string
    end
  end

  def up do
    alter table(:messages) do
      add :content, :text
      add :image_url, :string
    end

    flush()

    from(m in Message)
    |> Repo.stream()
    |> Stream.map(&migrate_message/1)
    |> Stream.run()

    execute "DROP VIEW conversations"

    alter table(:messages) do
      remove :message
    end

    execute """
    CREATE VIEW conversations
      (id, user_id, other_user_id, content, image_url, created_at, direction) AS
      (
        SELECT DISTINCT ON (
          min(sender_id, recipient_id),
          max(sender_id, recipient_id)
        )
          id,
          sender_id,
          recipient_id,
          content,
          image_url,
          created_at,
          'outgoing'
        FROM messages
        GROUP BY created_at, id, sender_id, recipient_id
        ORDER BY
          min(sender_id, recipient_id),
          max(sender_id, recipient_id),
          created_at desc
      )
      UNION ALL
      (
        SELECT DISTINCT ON (
          min(sender_id, recipient_id),
          max(sender_id, recipient_id)
        )
          id,
          recipient_id,
          sender_id,
          content,
          image_url,
          created_at,
          'incoming'
        FROM messages
        GROUP BY created_at, id, sender_id, recipient_id
        ORDER BY
          min(sender_id, recipient_id),
          max(sender_id, recipient_id),
          created_at desc
      )
    """
  end

  defp migrate_message(%Message{} = message) do
    content = xpath(message.message, ~x"//message/body/text()"S)
    image_url = xpath(message.message, ~x"//message/image/url/text()"S)

    message
    |> Changeset.change(content: content)
    |> Changeset.change(image_url: image_url)
    |> Repo.update()
  end
end
