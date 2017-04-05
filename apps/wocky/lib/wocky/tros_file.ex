defmodule Wocky.TROSFile do
  @moduledoc ""

  use Wocky.Repo.Model

  import Ecto.Changeset
  import Ecto.Query, only: [from: 2]

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
    user_id: User.id,
    access:  access
  }

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

  @spec get_user_id(id) :: User.id | nil
  def get_user_id(id) do
    TROSFile
    |> with_file(id)
    |> select_user_id
    |> Repo.one
  end

  @spec get_access(id) :: access | nil
  def get_access(id) do
    TROSFile
    |> with_file(id)
    |> select_access
    |> Repo.one
  end

  @change_fields [:access]

  defp changeset(struct, params \\ %{}) do
    struct
    |> cast(params, @change_fields)
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
