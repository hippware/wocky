defmodule WockyAPI.Schema.MediaTypes do
  @moduledoc """
  Absinthe types for media fields
  """

  use Absinthe.Schema.Notation

  object :media do
    field :tros_url, :string
    field :full_url, :string
    field :thumbnail_url, :string
  end
end
