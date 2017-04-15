defmodule Wocky.Repo.Migrations.AddUserFields do
  use Wocky.Repo.Migration

  def up do
    drop table(:users)

    create table(:users, primary_key: false) do
      add :id,           :uuid, primary_key: true
      add :username,     :string, null: false
      add :server,       :string, null: false
      add :external_id,  :string, null: false
      add :handle,       :string
      add :avatar,       :string
      add :first_name,   :string
      add :last_name,    :string
      add :phone_number, :string
      add :email,        :string
      add :password,     :text
      add :pass_details, :text

      timestamps()
    end

    create unique_index(:users, [:username])
    create unique_index(:users, [:external_id])
    create unique_index(:users, [:handle])
  end

  def down do
    drop unique_index(:users, [:username])
    drop unique_index(:users, [:external_id])
    drop unique_index(:users, [:handle])

    drop table(:users)

    execute """
    CREATE TABLE users (\
    username varchar(250) PRIMARY KEY, \
    password text NOT NULL, \
    pass_details text, \
    created_at timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP\
    ) CHARACTER SET utf8;\
    """
  end
end
