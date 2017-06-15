defmodule Wocky.Repo.Migrations.DeviceChanges do
  use Ecto.Migration

  def change do
    alter table(:devices), do: remove :endpoint
    rename table(:devices), :device, to: :token
  end
end
