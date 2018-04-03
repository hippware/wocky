defmodule WockyAPI.Type.UUID do
  @moduledoc """
  Absinthe scalar type encapsulating UUID IDs
  """

  use Absinthe.Schema.Notation

  alias Absinthe.Blueprint.Input.String
  alias Absinthe.Blueprint.Input.Null
  alias Wocky.Repo.ID

  scalar :uuid, name: "DateTime" do
    serialize fn uuid -> uuid end
    parse &parse_uuid/1
  end

  defp parse_uuid(%String{value: value}) do
    case ID.valid?(value) do
      true -> {:ok, value}
      false -> :error
    end
  end
  defp parse_uuid(%Null{}), do: nil
  defp parse_uuid(_), do: :error
end
