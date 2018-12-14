defmodule Wocky.Repo.Validations do
  @moduledoc "Reusable schema validations"

  import Ecto.Changeset

  def validate_required_inclusion(changeset, fields) do
    if Enum.any?(fields, &present?(changeset, &1)) do
      changeset
    else
      # Add the error to the first field only since Ecto requires
      # a field name for each error.
      add_error(
        changeset,
        hd(fields),
        "one of these fields must be present: #{inspect(fields)}"
      )
    end
  end

  defp present?(changeset, field) do
    value = get_field(changeset, field)
    value && value != ""
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
