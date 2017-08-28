defmodule Wocky.Repo.ID do
  @moduledoc """
  Responsible for generating unique IDs for records stored in a database.
  """

  alias Ecto.UUID

  @type t :: binary

  @doc "Generates a UUID in canonical text format for use as an id."
  @spec new(:v1 | :v4) :: t
  def new(type \\ :v1), do: :ossp_uuid.make(type, :text)

  @doc """
  Convert a binary UUID to a string.
  Raises an error if the binary is not a valid UUID.
  """
  @spec to_string!(t) :: t | no_return
  def to_string!(id), do: :ossp_uuid.import(id, :text)

  @doc "Convert a binary UUID to a string."
  @spec to_string(t) :: {:ok, t} | {:error, :invalid}
  def to_string(id) do
    {:ok, to_string!(id)}
  rescue
    ArgumentError -> {:error, :invalid}
  end

  @doc """
  Convert a string UUID to a binary.
  Raises an error if the string is not a valid UUID.
  """
  @spec to_binary!(t) :: t | no_return
  def to_binary!(id), do: :ossp_uuid.import(id, :binary)

  @doc "Convert a string UUID to a binary."
  @spec to_binary(t) :: {:ok, t} | {:error, :invalid}
  def to_binary(id) do
    {:ok, to_binary!(id)}
  rescue
    ArgumentError -> {:error, :invalid}
  end

  @doc "Returns true if the ID is a valid UUID."
  @spec valid?(t) :: boolean
  def valid?(id) do
    case UUID.dump(id) do
      {:ok, _} -> true
      :error ->
        case UUID.load(id) do
          {:ok, _} -> true
          :error -> false
        end
    end
  end
end
