defmodule WockyAPI.Schema.MetadataTypes do
  @moduledoc "Schema definition for server metadata interface"

  use WockyAPI.Schema.Notation

  alias WockyAPI.Resolvers.Metadata

  @desc "A single item of server metadata"
  object :metadatum do
    @desc "The datum's key"
    field :key, :string

    @desc "The datum's value"
    field :value, :string

    @desc "A human-readable description of the data"
    field :description, :string
  end

  object :metadata_queries do
    @desc "Retrieve the server's current metadata set"
    field :metadata, list_of(non_null(:metadatum)) do
      resolve &Metadata.get/2
    end
  end
end
