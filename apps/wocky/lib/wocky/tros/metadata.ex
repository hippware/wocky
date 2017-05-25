defmodule Wocky.TROS.Metadata do
  @moduledoc """
  DB interface module for TROS metadata (access and ownership info)
  """

  use Wocky.Repo.Model

  alias Wocky.User

  alias __MODULE__, as: TROSMetadata

  @primary_key false
  @foreign_key_type :binary_id
  schema "tros_metadatas" do
    field :id,     :binary_id, primary_key: true
    field :access, :binary, default: ""

    belongs_to :user, User

    timestamps()
  end

  @type id :: binary
  @type access :: binary

  @type t :: %TROSMetadata{
    id:      id,
    user_id: User.id,
    access:  access
  }

  @change_fields [:id, :user_id, :access]

  @spec put(id, User.id, access) :: {:ok, t}
  def put(id, user_id, access) do
    %TROSMetadata{}
    |> changeset(%{id: id, user_id: user_id, access: access})
    |> Repo.insert
  end

  @spec set_access(id, access) :: {:ok, t} | {:error, :not_found}
  def set_access(id, access) do
    case Repo.get(TROSMetadata, id) do
      nil ->
        {:error, :not_found}
      md ->
        md
        |> changeset(%{access: access})
        |> Repo.update
    end
  end

  @spec get(id) :: t | nil
  def get(id) do
    TROSMetadata
    |> with_file(id)
    |> Repo.one
  end

  @spec get_user_id(id) :: User.id | nil
  def get_user_id(id) do
    TROSMetadata
    |> with_file(id)
    |> select_user_id
    |> Repo.one
  end

  @spec get_access(id) :: access | nil
  def get_access(id) do
    TROSMetadata
    |> with_file(id)
    |> select_access
    |> Repo.one
  end

  @spec delete(id) :: :ok
  def delete(id) do
    TROSMetadata
    |> with_file(id)
    |> Repo.delete_all

    :ok
  end

  defp changeset(struct, params) do
    struct
    |> cast(params, @change_fields)
    |> unique_constraint(:id, name: :tros_metadatas_pkey)
    |> foreign_key_constraint(:user_id)
  end

  defp with_file(query, id) do
    from f in query, where: f.id == ^id
  end

  defp select_user_id(query) do
    from f in query, select: f.user_id
  end

  defp select_access(query) do
    from f in query, select: f.access
  end
end
