defmodule Wocky.HomeStreamItem do
  @moduledoc """
  DB interface module for home stream items
  """

  use Wocky.Repo.Model

  alias Wocky.User
  alias __MODULE__, as: HomeStreamItem

  @foreign_key_type :binary_id
  schema "home_stream_items" do
    field :key,      :string
    field :from_jid, :binary
    field :stanza,   :binary
    field :deleted,  :boolean, default: false

    belongs_to :user, User

    timestamps()
  end

  @type key :: binary

  @type t :: %HomeStreamItem{
    user_id:    User.id,
    key:        key,
    from_jid:   binary,
    stanza:     binary,
    deleted:    boolean,
    updated_at: DateTime::t
  }

  @change_fields [:user_id, :key, :from_jid, :stanza, :deleted]

  @doc "Write a home stream item to the database"
  @spec put(User.id, key, binary, binary) :: {:ok, t}
  def put(user_id, key, from_jid, stanza) do
    fields = %{
      user_id: user_id,
      key: key,
      from_jid: from_jid,
      stanza: stanza
    }
    result =
      %HomeStreamItem{}
      |> changeset(fields)
      |> Repo.insert!(on_conflict: :replace_all)
    {:ok, result}
  end

  @doc "Mark a single item as deleted"
  @spec delete(User.id, key) :: {:ok, t | nil}
  def delete(user_id, key) do
    item = Repo.get_by(HomeStreamItem, %{user_id: user_id, key: key})
    if item != nil do
      item
      |> changeset(%{deleted: true})
      |> Repo.update
    else
      {:ok, nil}
    end
  end

  @doc "Get all home stream items for a user"
  @spec get(User.id) :: [t]
  def get(user_id) do
    HomeStreamItem
    |> with_user(user_id)
    |> order_by_time()
    |> Repo.all
  end

  @doc "Get a single item by its key"
  @spec get_by_key(User.id, key) :: t | nil
  def get_by_key(user_id, key) do
    HomeStreamItem
    |> with_user(user_id)
    |> with_key(key)
    |> Repo.one
  end

  @doc "Get all items after a certain timestamp"
  @spec get_after_time(User.t, Datetime.t) :: [t]
  def get_after_time(user_id, time) do
    HomeStreamItem
    |> with_user(user_id)
    |> after_time(time)
    |> order_by_time()
    |> Repo.all
  end

  @doc "Get the latest timestamp for a user"
  @spec get_latest_time(User.id) :: Datetime.t
  def get_latest_time(user_id) do
    time =
      HomeStreamItem
      |> with_user(user_id)
      |> max_time()
      |> Repo.one
    if time == nil do
      DateTime.from_unix!(0)
    else
      time
    end
  end


  defp with_user(query, user_id) do
    from h in query, where: h.user_id == ^user_id
  end

  defp with_key(query, key) do
    from h in query, where: h.key == ^key
  end

  defp order_by_time(query) do
    from h in query, order_by: [asc: h.updated_at]
  end

  defp after_time(query, time) do
    from h in query, where: h.updated_at > ^time
  end

  defp max_time(query) do
    from h in query, select: max(h.updated_at)
  end

  defp changeset(struct, params) do
    cast(struct, params, @change_fields)
  end
end
