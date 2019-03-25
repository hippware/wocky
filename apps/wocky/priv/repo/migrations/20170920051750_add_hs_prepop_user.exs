defmodule Wocky.Repo.Migrations.AddHsPrepopUser do
  use Wocky.Repo.Migration

  # NOTE We are no longer doing HS prepopulation, making this migration a noop

  def up, do: :ok

  def down, do: :ok
end
