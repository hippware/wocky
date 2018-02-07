defmodule Wocky.Watcher.EventDecoder do
  @moduledoc """
  Functions to decode JSON from DB events into native Ecto structs
  """

  alias Ecto.Changeset
  alias WockyDBWatcher.Event

  def from_json(json, table_map) do
    json
    |> Poison.decode!(as: %Event{})
    |> fix_atoms()
    |> convert_objects(table_map)
  end

  defp fix_atoms(%Event{table: table, action: action} = event) do
    %{
      event
      | table: table,
        action: String.to_existing_atom(action)
    }
  end

  defp convert_objects(
         %Event{table: table, old: old, new: new} = event,
         table_map
       ) do
    case Map.get(table_map, table) do
      nil ->
        {nil, event}

      object ->
        {object,
         %{
           event
           | old: convert_object(object, old),
             new: convert_object(object, new)
         }}
    end
  end

  defp convert_object(_object, nil), do: nil

  defp convert_object(object, json) do
    object.__struct__
    |> Changeset.cast(json, object.__schema__(:fields))
    |> Changeset.apply_changes()
    |> maybe_fix(object)
  end

  defp maybe_fix(struct, object) do
    case function_exported?(object, :fix_from_json, 1) do
      true -> object.fix_from_json(struct)
      false -> struct
    end
  end
end
