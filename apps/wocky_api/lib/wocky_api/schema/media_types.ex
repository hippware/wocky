defmodule WockyAPI.Schema.MediaTypes do
  @moduledoc """
  Absinthe types for media fields
  """

  use WockyAPI.Schema.Notation

  alias WockyAPI.Resolvers.Media

  # -------------------------------------------------------------------
  # Objects

  @desc "Formats in which media can be downloaded"
  enum :media_format do
    @desc """
    The full size image, uncropped, up to 1920x1920 depending on the original's
    size
    """
    value :full

    @desc "A thumbnail of the image, cropped to a 360x360 square"
    value :thumbnail

    @desc """
    An uncropped thumbnail of the image with the aspect ratio maintained.
    Maximum large dimension of 360 pixels.
    """
    value :aspect_thumbnail
  end

  @desc "A URL for an image in a particular format"
  object :media_url do
    @desc "The type of image available at this URL"
    field :type, non_null(:media_format)

    @desc "The URL of the image in the specified format"
    field :url, non_null(:string)
  end

  @desc "A Wocky TROS media object"
  object :media do
    @desc "The TROS URL (invariant over the life of the object)"
    field :tros_url, :string

    @desc "The S3 URL for the full object (valid for 10 minutes)"
    field :full_url, :string, deprecate: "Please use the 'urls' list"

    @desc "The S3 URL for the thumbnail object (valid for 10 minutes)"
    field :thumbnail_url, :string, deprecate: "Please use the 'urls' list"

    @desc """
    A list of URLs for this image in different formats. If a format does not
    appear in the list, the image is not available in that format.
    """
    field :urls, non_null(list_of(non_null(:media_url)))
  end

  # -------------------------------------------------------------------
  # Queries

  object :media_queries do
    @desc "Request the newest retrieval URLS for a TROS file"
    field :media_urls, :media do
      @desc "The TROS URL of the object for which to retrieve URLs"
      arg :tros_url, non_null(:string)

      @desc """
      Time (in milliseconds) to wait before returning an error if the
      file has not become ready
      """
      arg :timeout, :integer

      resolve &Media.get_media_urls/2
    end
  end

  # -------------------------------------------------------------------
  # Mutations

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

  payload_object(:media_upload_payload, :media_upload_result)

  input_object :media_delete_params do
    field :url, non_null(:string)
  end

  payload_object(:media_delete_payload, :boolean)

  object :media_mutations do
    @desc "Request upload of a file"
    field :media_upload, type: :media_upload_payload do
      arg :input, non_null(:media_upload_params)
      resolve &Media.media_upload/2
      changeset_mutation_middleware()
    end

    @desc "Delete a file"
    field :media_delete, type: :media_delete_payload do
      arg :input, non_null(:media_delete_params)
      resolve &Media.media_delete/2
      changeset_mutation_middleware()
    end
  end
end
