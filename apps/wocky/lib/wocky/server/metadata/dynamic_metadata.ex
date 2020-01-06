defmodule Wocky.Server.Metadata.DynamicMetadata do
  @moduledoc "Dynamically generated server metadata interface"

  alias Wocky.Server.Metadata

  @providers [
    Wocky.Server.Metadata.ServerVersion
  ]

  @callback get :: {Metadata.key(), Metadata.value(), Metadata.description()}

  def get do
    Enum.map(@providers, & &1.get())
  end
end
