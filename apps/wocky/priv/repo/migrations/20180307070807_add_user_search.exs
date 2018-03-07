defmodule Wocky.Repo.Migrations.AddUserSearch do
  use Ecto.Migration

  def up do
    execute "CREATE EXTENSION unaccent"

    execute """
    CREATE FUNCTION users_name_fts(first_name text, last_name text)
    RETURNS tsvector AS $$
    SELECT to_tsvector('simple', unaccent(first_name)) ||
           to_tsvector('simple', unaccent(last_name));
    $$
    LANGUAGE sql
    IMMUTABLE
    """

    execute """
    CREATE INDEX users_name_fts ON users
    USING gin(users_name_fts(first_name, last_name))
    """
  end
end
