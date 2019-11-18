defmodule Wocky.TROS.Metadata do
  @moduledoc """
  DB interface module for TROS metadata (access and ownership info)
  """

  use Wocky.Repo.Schema

  import EctoEnum

  alias Wocky.Account.User

  defenum(FileTypeEnum, :file_type, [
    :full,
    :original,
    :thumbnail,
    :aspect_thumbnail
  ])

  @primary_key false
  @foreign_key_type :binary_id
  schema "tros_metadatas" do
    field :id, :binary_id, primary_key: true
    field :access, :binary, default: ""
    field :ready, :boolean
    field :available_formats, {:array, FileTypeEnum}

    belongs_to :user, User

    timestamps()
  end

  @type id :: String.t()
  @type access :: String.t()

  @type t :: %Metadata{
          id: id,
          user_id: User.id(),
          access: access,
          ready: boolean,
          available_formats: [FileTypeEnum.t()]
        }

  @change_fields [:id, :user_id, :access, :ready]

  @doc false
  def changeset(struct, params) do
    struct
    |> cast(params, @change_fields)
    |> unique_constraint(:id, name: :tros_metadatas_pkey)
    |> foreign_key_constraint(:user_id)
  end
end
