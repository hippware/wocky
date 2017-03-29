defmodule Golem.Schema do
  @moduledoc "Helper module to set some default Ecto schema options"

  defmacro __using__(_) do
    quote do
      use Ecto.Schema
      @timestamps_opts [
        inserted_at: :created_at,
        type: :utc_datetime,
        usec: true
      ]
    end
  end
end
