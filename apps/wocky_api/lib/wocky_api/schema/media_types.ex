defmodule WockyAPI.Schema.MediaTypes do
  @moduledoc """
  Absinthe types for media fields
  """

  use WockyAPI.Schema.Notation

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
end
