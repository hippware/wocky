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
    @desc "Name of the file being uploaded"
    field :filename, non_null(:string)

    @desc "Size of the file being uploaded in bytes"
    field :size, non_null(:integer)

    @desc "MIME type of the file being uploaded (eg image/jpeg)"
    field :mime_type, non_null(:string)

    @desc "Access string for the file being uploaded"
    field :access, :string
  end

  payload_object(:media_upload_payload, :media_upload_result)

  object :request_header do
    @desc "HTTP Header name"
    field :name, non_null(:string)

    @desc "HTTP Header value"
    field :value, non_null(:string)
  end

  object :media_upload_result do
    @desc "ID of the file to be uploaded"
    field :id, non_null(:uuid)

    @desc "Destination URL to which the file should be uploaded"
    field :upload_url, non_null(:string)

    @desc "HTTP method to use for upload request"
    field :method, non_null(:string)

    @desc "HTTP headers to include in upload request"
    field :headers, non_null(list_of(:request_header))

    @desc "TROS URL by which the file will be referred to in other objects"
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
