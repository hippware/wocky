defmodule Wocky.ID do
  @moduledoc """
  Responsible for generating unique IDs for records stored in a database.
  """

  @type t :: binary

  @doc "Generates a timeuuid in canonical text format for use as an id."
  @spec create :: t
  def create do
    :ossp_uuid.make(:v1, :text)
  end

  @doc "Returns true if the ID is a valid UUID."
  @spec valid?(t) :: boolean
  def valid?(id) do
    try do
      :ossp_uuid.import(id, :binary)
      true
    rescue
      _ -> false
    end
  end
end
