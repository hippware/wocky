defmodule Wocky.StringList do
  @moduledoc """
  Custom ecto type for (de)serialising string lists
  """

  @behaviour Ecto.Type

  def type, do: :binary

  def cast(string_list) when is_list(string_list) do
    {:ok, Enum.join(string_list, <<0>>)}
  end
  def cast(_), do: :error

  def load(string) do
    {:ok, String.split(string, <<0>>)}
  end

  def dump(string) when is_binary(string), do: {:ok, string}
  def dump(_), do: :error
end
