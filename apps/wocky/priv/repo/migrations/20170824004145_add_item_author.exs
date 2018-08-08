# credo:disable-for-this-file Credo.Check.Readability.MaxLineLength
defmodule Wocky.Repo.Migrations.AddItemAuthor do
  use Wocky.Repo.Migration

  import Ecto.Query

  alias Wocky.Repo

  def up do
    alter table(:bot_items) do
      add :user_id, references(:users, type: :uuid, on_delete: :delete_all, null: false)
    end

    flush()

    from(b in "bots")
    |> select([:id, :user_id])
    |> Repo.stream()
    |> Stream.each(&set_user(&1))
  end

  def down do
    alter table(:bot_items) do
      remove :user_id
    end
  end

  defp set_user(%{user_id: user_id, id: bot_id}) do
    execute "UPDATE bot_items SET user_id = #{user_id} WHERE bot_id = #{bot_id}"
  end
end
