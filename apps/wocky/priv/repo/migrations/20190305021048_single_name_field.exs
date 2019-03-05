defmodule Wocky.Repo.Migrations.SingleNameField do
  use Ecto.Migration

  def up do
    alter table(:users) do
      add :name, :text
    end

    flush()

    execute """
    UPDATE users SET name = trim(both from concat(first_name, ' ', last_name))
    """

    execute """
    DROP INDEX users_name_fts
    """

    execute """
    DROP FUNCTION users_name_fts(text, text, text)
    """

    execute """
    CREATE FUNCTION users_name_fts(name text, handle text)
    RETURNS tsvector AS $$
    SELECT to_tsvector('simple', unaccent(name)) ||
           to_tsvector('simple', unaccent(handle));
    $$
    LANGUAGE sql
    IMMUTABLE
    """

    execute """
    CREATE INDEX users_name_fts ON users USING gin(users_name_fts(name, handle))
    """

    alter table(:users) do
      remove :first_name
      remove :last_name
    end
  end
end
