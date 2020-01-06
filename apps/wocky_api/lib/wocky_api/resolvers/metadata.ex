defmodule WockyAPI.Resolvers.Metadata do
  @moduledoc "Resolver for server metadata"

  alias Wocky.Server

  def get(_, _) do
    {:ok,
     Server.get_metadata()
     |> Enum.map(fn {key, value, description} ->
       %{key: key, value: value, description: description}
     end)}
  end
end
