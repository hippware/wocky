defmodule Wocky.Repo.Migrations.AddMessagesTable do
  use Wocky.Repo.Migration

  import Ecto.Query

  require Bitwise

  alias Wocky.Repo
  alias Wocky.Repo.Migration.Utils

  def up do
    execute "DROP VIEW messages"

    create table(:messages) do
      add :sender_id, references(:users, type: :uuid, on_delete: :delete_all), null: false
      add :recipient_id, references(:users, type: :uuid, on_delete: :delete_all), null: false

      add :message, :text, null: false

      # Hold on to the old ID for now in case we want to re-run the
      # migration without clobbering stuff
      add :migration_id, :bigint

      Wocky.Repo.Migration.timestamps(updated_at: false)
    end

    create index(:messages, [:sender_id])
    create index(:messages, [:recipient_id])

    flush()

    migrate_messages()

    Utils.update_notify(:messages, [:insert])
  end

  defp migrate_messages do
    from(m in "mam_message",
      inner_join: su in "mam_server_user", on: m.user_id == su.id,
      inner_join: sender in "users", on: fragment("uuid(?)", su.user_name) == sender.id,
      inner_join: recipient in "users", on: fragment("uuid(?)", m.remote_bare_jid) == recipient.id,
      select: %{
        :id => m.id,
        :sender_id => sender.id,
        :recipient_id => recipient.id,
        :message => m.message
      },
      where: m.direction == "O"
    )
    |> Repo.stream()
    |> Stream.map(&migrate_message/1)
    |> Stream.filter(&(Keyword.get(&1, :created_at).year > 1980))
    |> Stream.each(&Repo.insert_all("messages", [&1]))
    |> Stream.run()
  end

  defp migrate_message(m) do
    [
      sender_id: m.sender_id,
      recipient_id: m.recipient_id,
      message: m.message |> :erlang.binary_to_term() |> :exml.to_binary(),
      migration_id: m.id,

      # See mod_mam_utils:generate_message_id/0 - we're reversing that process
      # to get a timestamp
      created_at: m.id |> Bitwise.bsr(8) |> Timex.from_unix(:microseconds)
    ]
  end


end
