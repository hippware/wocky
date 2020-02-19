defmodule Wocky.Repo.Migrations.MoveNotificationDataToMap do
  use Wocky.Repo.Migration

  alias Wocky.Repo

  import Ecto.Query

  require Logger

  defmodule Notification do
    defstruct [
      :id
    ]
  end

  @remove [
    :bot_invitation_accepted,
    :expires_at,
    :geofence_event,
    :share_id
  ]

  def up do
    alter table(:notifications) do
      add :data, :map, null: false, default: %{}
    end

    "notifications"
    |> select([n], ^[:id | @remove])
    |> Repo.stream()
    |> Stream.each(&migrate/1)
    |> Stream.run()

    alter table(:notifications) do
      Enum.each(@remove, &remove/1)
    end
  end

  defp migrate(row) do
    data =
      row
      |> Enum.reject(fn {_, v} -> is_nil(v) end)
      |> Enum.into(%{})
      |> Map.drop([:id])
      |> Jason.encode!()

    execute(fn ->
      repo().query!(
        "UPDATE notifications SET data = $1::jsonb WHERE id = $2",
        [data, row.id],
        log: false
      )
    end)
  end
end
