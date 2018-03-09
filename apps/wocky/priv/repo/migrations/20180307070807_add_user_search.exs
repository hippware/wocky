defmodule Wocky.Repo.Migrations.AddUserSearch do
  use Ecto.Migration

  def up do
    execute "CREATE EXTENSION unaccent"

    execute """
    CREATE FUNCTION users_name_fts(first_name text, last_name text, handle text)
    RETURNS tsvector AS $$
    SELECT to_tsvector('simple', unaccent(first_name)) ||
           to_tsvector('simple', unaccent(last_name)) ||
           to_tsvector('simple', unaccent(handle));
    $$
    LANGUAGE sql
    IMMUTABLE
    """

    execute """
    CREATE INDEX users_name_fts ON users
    USING gin(users_name_fts(first_name, last_name, handle))
    """
  end
end
