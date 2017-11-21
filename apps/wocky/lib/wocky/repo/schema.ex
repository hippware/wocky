defmodule Wocky.Repo.Schema do
  @moduledoc "Helper module to set some default Ecto schema options"

  defmacro __using__(_) do
    quote do
      use Ecto.Schema

      import Ecto.Changeset

      alias __MODULE__

      @timestamps_opts [
        inserted_at: :created_at,
        type: :utc_datetime,
        usec: true
      ]
    end
  end
end
