defmodule Wocky.Server do
  @moduledoc """
  Server metadata access
  """

  alias Wocky.Repo
  alias Wocky.Server.Metadata
  alias Wocky.Server.Metadata.DynamicMetadata

  @spec get_metadata :: [%Metadata{}]
  def get_metadata do
    Metadata
    |> Repo.all()
    |> Enum.map(fn m -> {m.key, m.value, m.description} end)
    |> Enum.concat(DynamicMetadata.get())
  end
end
