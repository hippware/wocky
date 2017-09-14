defmodule Wocky.HomeStreamItem do
  @moduledoc """
  DB interface module for home stream items
  """

  use Wocky.Repo.Model

  alias Wocky.Bot
  alias Wocky.User
  alias __MODULE__, as: HomeStreamItem

  @foreign_key_type :binary_id
  schema "home_stream_items" do
    field :key,      :string
    field :from_jid, :binary, default: ""
    field :stanza,   :binary, default: ""
    field :deleted,  :boolean, default: false

    belongs_to :user, User
    belongs_to :reference_user, User, foreign_key: :reference_user_id
    belongs_to :reference_bot, Bot, foreign_key: :reference_bot_id

    timestamps()
  end

  @type key :: binary

  @type t :: %HomeStreamItem{
    user_id:        User.id,
    key:            key,
    from_jid:       binary,
    stanza:         binary,
    deleted:        boolean,
    updated_at:     DateTime.t,
    reference_user: User.t,
    reference_bot:  Bot.t
  }

  @change_fields [:user_id, :key, :from_jid, :stanza, :deleted,
                  :reference_user_id, :reference_bot_id]

  @delete_changes [deleted: true, stanza: "", from_jid: "",
                   reference_user_id: nil, reference_bot_id: nil]

  @doc "Write a home stream item to the database"
  @spec put(User.id, key, binary, binary, Keyword.t)
  :: {:ok, t} | {:error, term}
  def put(user_id, key, from_jid, stanza, opts \\ []) do
    fields = %{
      user_id: user_id,
      key: key,
      from_jid: from_jid,
      stanza: stanza,
      reference_user_id: Keyword.get(opts, :ref_user_id),
      reference_bot_id: Keyword.get(opts, :ref_bot_id)
    }

    %HomeStreamItem{}
    |> changeset(fields)
    |> Repo.insert(on_conflict: :replace_all,
                   conflict_target: [:user_id, :key])
  end

  @doc "Mark a single item as deleted"
  @spec delete(User.id, key) :: {:ok, t | nil}
  def delete(user_id, key) when is_binary(key) do
    item = Repo.get_by(HomeStreamItem, %{user_id: user_id, key: key})
    if item != nil do
      item
      |> changeset(Map.new(@delete_changes))
      |> Repo.update
    else
      {:ok, nil}
    end
  end

  @spec delete(User.t, User.t | Bot.t) :: {:ok, t | nil}
  def delete(%User{id: user_id}, %User{id: ref_user_id}) do
    HomeStreamItem
    |> where(user_id: ^user_id)
    |> where(reference_user_id: ^ref_user_id)
    |> Repo.update_all(set: @delete_changes)
    :ok
  end

  def delete(%User{id: user_id}, %Bot{id: ref_bot_id}) do
    HomeStreamItem
    |> where(user_id: ^user_id)
    |> where(reference_bot_id: ^ref_bot_id)
    |> Repo.update_all(set: @delete_changes)
    :ok
  end

  @spec delete_by_user_ref(User.t) :: :ok
  def delete_by_user_ref(user) do
    HomeStreamItem
    |> where(reference_user_id: ^user.id)
    |> Repo.update_all(set: @delete_changes)
    :ok
  end

  @spec delete_by_user_ref(Bot.t) :: :ok
  def delete_by_bot_ref(bot) do
    HomeStreamItem
    |> where(reference_bot_id: ^bot.id)
    |> Repo.update_all(set: @delete_changes)
    :ok
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
  @spec get_after_time(User.id, Datetime.t) :: [t]
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

  def with_user(user_id), do: with_user(HomeStreamItem, user_id)


  def with_user(query, user_id) do
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
    struct
    |> cast(params, @change_fields)
    |> foreign_key_constraint(:reference_user_id)
    |> foreign_key_constraint(:reference_bot_id)
  end
end
