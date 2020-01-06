defmodule WockyAPI.Schema.MetadataTypes do
  @moduledoc "Schema definition for server metadata interface"

  use WockyAPI.Schema.Notation

  alias WockyAPI.Resolvers.Metadata

  object :metadatum do
    field :key, :string
    field :value, :string
    field :description, :string
  end

  object :metadata_queries do
    field :metadata, list_of(non_null(:metadatum)) do
      resolve &Metadata.get/2
    end
  end
end
