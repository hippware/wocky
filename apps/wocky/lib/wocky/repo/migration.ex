defmodule Wocky.Repo.Migration do
  @moduledoc "Helper module to make migrations easier"

  alias Ecto.Migration

  defmacro __using__(_) do
    quote do
      import Ecto.Migration, except: [timestamps: 0]
      import Wocky.Repo.Migration
      @disable_ddl_transaction false
      @disable_migration_lock false
      @before_compile Ecto.Migration

      alias unquote(__MODULE__)
    end
  end

  import Ecto.Migration, except: [timestamps: 0]

  @spec timestamps(Keyword.t()) :: :ok | nil
  def timestamps(overrides \\ []) do
    [inserted_at: :created_at, type: :timestamptz]
    |> Keyword.merge(overrides)
    |> Migration.timestamps()
  end

  @spec reset_enum(any(), [String.t()]) :: :ok
  def reset_enum(enum, table_cols) do
    execute "ALTER TYPE #{enum.schemaless_type()} RENAME TO #{
              enum.schemaless_type()
            }_old"

    enum.create_type()

    Enum.each(
      table_cols,
      fn {table, col, default} ->
        if default,
          do: execute("ALTER TABLE #{table} ALTER COLUMN #{col} DROP DEFAULT")

        execute "ALTER TABLE #{table} ALTER COLUMN #{col} TYPE #{enum.type()} USING #{
                  col
                }::text::#{enum.type()}"

        if default,
          do:
            execute(
              "ALTER TABLE #{table} ALTER COLUMN #{col} SET DEFAULT '#{default}'::#{
                enum.type()
              }"
            )
      end
    )

    execute "DROP TYPE #{enum.schemaless_type()}_old"
  end
end
