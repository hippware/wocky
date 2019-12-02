defmodule Wocky.Repo.Schema do
  @moduledoc "Helper module to set some default Ecto schema options"

  defmacro __using__(_) do
    quote do
      use Ecto.Schema

      import Ecto.Changeset
      import Wocky.Repo.Validations

      alias Ecto.Changeset

      @timestamps_opts [
        inserted_at: :created_at,
        type: :utc_datetime_usec
      ]
    end
  end
end
