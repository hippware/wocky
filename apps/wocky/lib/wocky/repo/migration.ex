defmodule Wocky.Repo.Migration do
  @moduledoc "Helper module to make migrations easier"

  alias Ecto.Migration

  defmacro __using__(_) do
    quote do
      import Ecto.Migration, except: [timestamps: 0]
      import Wocky.Repo.Migration
      @disable_ddl_transaction false
      @before_compile Ecto.Migration
    end
  end

  def timestamps(overrides \\ []) do
    [inserted_at: :created_at, type: :utc_datetime, size: 6]
    |> Keyword.merge(overrides)
    |> Migration.timestamps()
  end
end
