defmodule Wocky.Repo.Model do
  @moduledoc "Helper module to set some default Ecto schema options"

  defmacro __using__(_) do
    quote do
      use Ecto.Schema

      import Ecto
      import Ecto.Changeset
      import Ecto.Query

      alias Wocky.Repo

      @timestamps_opts [
        inserted_at: :created_at,
        type: :utc_datetime,
        usec: true
      ]
    end
  end
end
