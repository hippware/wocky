defmodule Wocky.Repo.Migrations.SetCapturedAt do
  use Wocky.Repo.Migration

  def up do
    execute """
    UPDATE user_locations SET captured_at = created_at WHERE captured_at IS NULL
    """
  end

  def down do
    # Nothing
  end
end
