defmodule Wocky.TROSFile do
  @moduledoc ""

  use Wocky.Repo.Model

  import Ecto.Changeset
  import Ecto.Query, only: [from: 2]

  alias Ecto.Multi
  alias Ecto.UUID
  alias Wocky.Repo
  alias Wocky.User

  alias __MODULE__, as: TROSFile

  @primary_key false
  schema "tros_metadata" do
    field :id,        :binary_id, primary_key: true
    field :user_id,   :binary_id
    field :access,    :binary

    timestamps()
  end

  @type id :: binary
  @type access :: binary

  @type t :: %TROSFile{
    id:      id,
    user_id: binary,
    access:  access
  }

  @doc ""
  @spec put(id, User.id, access) :: :ok
  def put(id, user_id, access) do
    Repo.insert!(%TROSFile{id: id, user_id: user_id, access: access})
    :ok
  end

  @spec set_access(id, access) :: :ok
  def set_access(id, access) do
    Repo.get!(TROSFile, id)
    |> changeset(%{access: access})
    |> Repo.update!
    :ok
  end

  @spec get_user_id(id) :: binary | nil
  def get_user_id(id) do
    case Repo.one(from f in get_by_id(id), select: f.user_id) do
      nil ->
        nil
      uuid ->
        {:ok, uuid} = UUID.load(uuid)
        uuid
    end
  end

  @spec get_access(id) :: access | nil
  def get_access(id) do
    Repo.one(from f in get_by_id(id), select: f.access)
  end

  @change_fields [:access]

  defp changeset(struct, params \\ %{}) do
    struct
    |> cast(params, @change_fields)
  end

  defp get_by_id(id) do
    from f in "tros_metadata", where: f.id == type(^id, :binary_id)
  end

end
