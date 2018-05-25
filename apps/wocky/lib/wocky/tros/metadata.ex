defmodule Wocky.TROS.Metadata do
  @moduledoc """
  DB interface module for TROS metadata (access and ownership info)
  """

  use Wocky.Repo.Schema

  import Ecto.Query

  alias Wocky.Repo
  alias Wocky.User
  alias __MODULE__, as: TROSMetadata

  @primary_key false
  @foreign_key_type :binary_id
  schema "tros_metadatas" do
    field :id, :binary_id, primary_key: true
    field :access, :binary, default: ""
    field :ready, :boolean

    belongs_to :user, User

    timestamps()
  end

  @type id :: binary
  @type access :: binary

  @type t :: %TROSMetadata{
          id: id,
          user_id: User.id(),
          access: access,
          ready: boolean
        }

  @change_fields [:id, :user_id, :access, :ready]

  @spec put(id, User.id(), access) :: {:ok, t} | {:error, any}
  def put(id, user_id, access) do
    %TROSMetadata{}
    |> changeset(%{id: id, user_id: user_id, access: access, ready: false})
    |> Repo.insert()
  end

  @spec set_access(id, access) :: {:ok, t} | {:error, :not_found}
  def set_access(id, access) do
    case Repo.get(TROSMetadata, id) do
      nil ->
        {:error, :not_found}

      md ->
        md
        |> changeset(%{access: access})
        |> Repo.update()
    end
  end

  def set_ready(id) do
    TROSMetadata
    |> Repo.get!(id)
    |> changeset(%{ready: true})
    |> Repo.update!()
  end

  @spec get(id) :: t | nil
  def get(id) do
    TROSMetadata
    |> with_file(id)
    |> Repo.one()
  end

  @spec get_user_id(id) :: User.id() | nil
  def get_user_id(id) do
    TROSMetadata
    |> with_file(id)
    |> select_user_id()
    |> Repo.one()
  end

  @spec get_access(id) :: access | nil
  def get_access(id) do
    TROSMetadata
    |> with_file(id)
    |> select_access()
    |> Repo.one()
  end

  @spec delete(id, User.t()) :: :ok
  def delete(id, %User{id: user_id}) do
    case get(id) do
      %Metadata{user_id: ^user_id} = metadata ->
        Repo.delete(metadata)
        :ok

      nil ->
        {:error, :not_found}

      _ ->
        {:error, :permission_denied}
    end
  end

  @spec ready?(id) :: boolean
  def ready?(id) do
    # Return false for nil result
    TROSMetadata
    |> with_file(id)
    |> select_ready()
    |> Repo.one()
    |> Kernel.==(true)
  end

  defp changeset(struct, params) do
    struct
    |> cast(params, @change_fields)
    |> unique_constraint(:id, name: :tros_metadatas_pkey)
    |> foreign_key_constraint(:user_id)
  end

  defp with_file(query, id), do: from(f in query, where: f.id == ^id)

  defp select_user_id(query), do: from(f in query, select: f.user_id)
  defp select_access(query), do: from(f in query, select: f.access)
  defp select_ready(query), do: from(f in query, select: f.ready)
end
