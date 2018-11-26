defmodule Wocky.Repo.ID do
  @moduledoc """
  Responsible for generating unique IDs for records stored in a database.
  """

  alias Ecto.UUID

  @type t :: binary

  @doc "Generates a UUID in canonical text format for use as an id."
  @spec new :: t
  def new, do: UUID.generate()

  @doc "Returns true if the ID is a valid UUID."
  @spec valid?(t) :: boolean
  def valid?(id) do
    case UUID.dump(id) do
      {:ok, _} -> true
      :error -> false
    end
  end
end
