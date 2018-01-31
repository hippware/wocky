defmodule Wocky.Watcher.EventDecoder do
  @moduledoc """
  Functions to decode JSON from DB events into native Ecto structs
  """

  alias Ecto.Changeset
  alias WockyDBWatcher.Event

  def from_json(json) do
    json
    |> Poison.decode!(as: %Event{})
    |> fix_atoms()
    |> convert_objects()
  end

  defp fix_atoms(%Event{object: object, action: action} = event) do
    %{
      event
      | object: String.to_existing_atom(object),
        action: String.to_existing_atom(action)
    }
  end

  defp convert_objects(%Event{object: object, old: old, new: new} = event) do
    %{
      event
      | old: convert_object(object, old),
        new: convert_object(object, new)
    }
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
