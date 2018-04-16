defmodule WockyAPI.Types.AInt do
  @moduledoc """
  Absinthe scalar type encapsulating arbitrary precision integer
  """

  use Absinthe.Schema.Notation

  alias Absinthe.Blueprint.Input.Null
  alias Absinthe.Blueprint.Input.String, as: InputString

  scalar :aint, name: "AInt" do
    serialize fn aint -> Integer.to_string(aint) end
    parse &parse_aint/1
  end

  defp parse_aint(%InputString{value: value}) do
    case Integer.parse(value) do
      {int, ""} -> {:ok, int}
      _ -> :error
    end
  end

  defp parse_aint(%Null{}), do: {:ok, nil}
  defp parse_aint(_), do: :error
end
