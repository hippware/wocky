defmodule Wocky.Repo.Migrations.ExtendTokenExpiry do
  use Wocky.Repo.Migration

  def up do
    execute "update tokens set expires_at = expires_at + interval '46 days';"
  end

  def down do
    execute "update tokens set expires_at = expires_at - interval '46 days';"
  end
end
