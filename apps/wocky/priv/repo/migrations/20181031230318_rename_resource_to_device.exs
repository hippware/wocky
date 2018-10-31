defmodule Wocky.Repo.Migrations.RenameResourceToDevice do
  use Ecto.Migration

  def change do
    rename table(:push_logs), :resource, to: :device
    rename table(:push_tokens), :resource, to: :device
    rename table(:tokens), :resource, to: :device
    rename table(:traffic_logs), :resource, to: :device
    rename table(:user_bot_events), :resource, to: :device
    rename table(:user_locations), :resource, to: :device
  end
end
