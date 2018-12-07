defmodule Wocky.Repo.Migrations.ConsistentImageUrl do
  use Ecto.Migration

  def change do
    rename table(:bots), :image, to: :image_url
    rename table(:users), :avatar, to: :image_url
  end
end
