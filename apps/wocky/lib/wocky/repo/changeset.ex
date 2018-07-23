defmodule Wocky.Repo.Changeset do
  @moduledoc "Helper functions for Ecto changeset management"

  import Ecto.Changeset

  defmacro __using__(_) do
    quote do
      import unquote(__MODULE__)
    end
  end

  def validate_not_nil(changeset, fields) when is_list(fields) do
    Enum.reduce(fields, changeset, fn field, changeset ->
      if get_field(changeset, field) == nil do
        add_error(changeset, field, "nil")
      else
        changeset
      end
    end)
  end

  def validate_not_nil(changeset, field),
    do: validate_not_nil(changeset, [field])
end
