defmodule WockyAPI.Schema.MediaTypes do
  @moduledoc """
  Absinthe types for media fields
  """

  use WockyAPI.Schema.Notation

  import Kronky.Payload

  alias WockyAPI.Resolvers.Media

  @desc "A Wocky TROS media object"
  object :media do
    scope :public

    @desc "The TROS URL (invariant over the life of the object)"
    field :tros_url, :string

    @desc "The S3 URL for the full object (valid for 10 minutes)"
    field :full_url, :string

    @desc "The S3 URL for the thumbnail object (valid for 10 minutes)"
    field :thumbnail_url, :string
  end

  input_object :media_upload_params do
    field :filename, non_null(:string)
    field :size, non_null(:integer)
    field :mime_type, non_null(:string)
    field :access, :string
  end

  payload_object(:media_upload_payload, :media_upload_result)

  object :request_header do
    field :name, non_null(:string)
    field :value, non_null(:string)
  end

  object :media_upload_result do
    field :id, non_null(:uuid)
    field :upload_url, non_null(:string)
    field :method, non_null(:string)
    field :headers, non_null(list_of(:request_header))
    field :reference_url, non_null(:string)
  end

  object :media_mutations do
    @desc "Request upload of a file"
    field :media_upload, type: :media_upload_payload do
      arg :input, non_null(:media_upload_params)
      resolve &Media.upload/3
      changeset_mutation_middleware
    end
  end
end
