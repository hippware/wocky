defmodule Wocky.HomeStreamItem do
  @moduledoc """
  DB interface module for home stream items
  """

  use Wocky.Repo.Model

  alias Timex.Duration
  alias Wocky.Bot
  alias Wocky.Bot.Share
  alias Wocky.User
  alias __MODULE__, as: HomeStreamItem

  @foreign_key_type :binary_id
  schema "home_stream_items" do
    field :key,      :string
    field :from_jid, :binary, default: ""
    field :stanza,   :binary, default: ""
    field :deleted,  :boolean, default: false
    field :ordering, :utc_datetime

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
    ordering:       DateTime.t,
    reference_user: User.t,
    reference_bot:  Bot.t
  }

  @change_fields [:user_id, :key, :from_jid, :stanza, :deleted,
                  :reference_user_id, :reference_bot_id, :ordering]

  @prepopulate_fields @change_fields ++ [:created_at, :updated_at]

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
      reference_bot_id: Keyword.get(opts, :ref_bot_id),
      ordering: Keyword.get(opts, :ordering, DateTime.utc_now())
    }

    conflict_set =
      fields
      |> conflict_set(Keyword.get(opts, :set_ordering, true))
      |> Map.to_list

    %HomeStreamItem{}
    |> changeset(fields)
    |> Repo.insert(on_conflict: [set: conflict_set],
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
    |> Repo.update_all(set: delete_changes())
    :ok
  end

  def delete(%User{id: user_id}, %Bot{id: ref_bot_id}) do
    HomeStreamItem
    |> where(user_id: ^user_id)
    |> where(reference_bot_id: ^ref_bot_id)
    |> Repo.update_all(set: delete_changes())
    :ok
  end

  @spec delete_by_user_ref(User.t) :: :ok
  def delete_by_user_ref(user) do
    HomeStreamItem
    |> where(reference_user_id: ^user.id)
    |> Repo.update_all(set: delete_changes())
    :ok
  end

  @spec delete_by_bot_ref(Bot.t) :: :ok
  def delete_by_bot_ref(bot) do
    HomeStreamItem
    |> where(reference_bot_id: ^bot.id)
    |> Repo.update_all(set: delete_changes())
    :ok
  end

  @doc """
  Marks as deleted all home stream items referencing the given bot where the
  item's owner is not able to view the bot
  """
  @spec delete_by_bot_ref_invisible(Bot.t) :: :ok
  def delete_by_bot_ref_invisible(%Bot{public: true}), do: :ok
  def delete_by_bot_ref_invisible(bot) do
    Repo.transaction fn ->
      HomeStreamItem
      |> join(:left, [i], s in Share,
              s.user_id == i.user_id
              and s.bot_id == ^bot.id)
      |> where([i, s],
               i.reference_bot_id == ^bot.id
               and is_nil(s.user_id)
               and i.user_id != ^bot.user_id)
      |> Repo.stream
      |> Stream.each(fn(item) ->
        item
        |> changeset(Map.new(@delete_changes))
        |> Repo.update
      end)
      |> Stream.run
    end
    :ok
  end

  @doc "Get all home stream items for a user"
  @spec get(User.id, boolean) :: [t]
  def get(user_id, exclude_deleted \\ false, update_ordering \\ false) do
    user_id
    |> get_query(exclude_deleted)
    |> set_order(update_ordering)
    |> Repo.all
  end

  @doc "Get a single item by its key"
  @spec get_by_key(User.id, key, boolean) :: t | nil
  def get_by_key(user_id, key, exclude_deleted \\ false) do
    user_id
    |> get_query(exclude_deleted)
    |> where(key: ^key)
    |> Repo.one
  end

  @doc "Get all items after a certain timestamp"
  @spec get_after_time(User.id, DateTime.t | binary) :: [t]
  def get_after_time(user_id, time) do
    user_id
    |> get_query(false)
    |> where([i], i.updated_at > ^time)
    |> set_order(true)
    |> Repo.all
  end

  @doc "Get the latest version for a user"
  @spec get_latest_version(User.id) :: DateTime.t
  def get_latest_version(user_id) do
    time =
      HomeStreamItem
      |> with_user(user_id)
      |> select([i], max(i.updated_at))
      |> Repo.one
    if time == nil do
      DateTime.from_unix!(0)
    else
      time
    end
  end

  @spec get_query(User.id, boolean) :: Ecto.Queryable.t
  def get_query(user_id, exclude_deleted \\ false) do
    HomeStreamItem
    |> with_user(user_id)
    |> preload(:reference_bot)
    |> maybe_exclude_deleted(exclude_deleted)
  end

  @spec prepopulate_from(User.id, User.id, Duration.t) :: :ok
  def prepopulate_from(user_id, from_id, period) do
    from_time = Timex.subtract(DateTime.utc_now(), period)

    from_id
    |> get_after_time(from_time)
    |> Enum.each(
      fn(i) ->
        params =
          i
          |> Map.take(@prepopulate_fields)
          |> Map.put(:user_id, user_id)

        %HomeStreamItem{}
        |> cast(params, @prepopulate_fields)
        |> foreign_key_constraint(:reference_user_id)
        |> foreign_key_constraint(:reference_bot_id)
        |> Repo.insert(on_conflict: :replace_all,
                       conflict_target: [:user_id, :key])
      end)
  end

  @spec bump_version_by_ref_bot(Bot.t) :: [HomeStreamItem.t]
  def bump_version_by_ref_bot(ref_bot) do
    HomeStreamItem
    |> where(reference_bot_id: ^ref_bot.id)
    |> Repo.update_all([set: [updated_at: DateTime.utc_now]], returning: true)
    |> elem(1)
    |> Repo.preload(:user)
  end

  def with_user(user_id), do: with_user(HomeStreamItem, user_id)

  def with_user(query, user_id) do
    query
    |> where(user_id: ^user_id)
  end

  defp set_order(query, true) do
    query
    |> order_by(asc: :updated_at)
  end
  defp set_order(query, false) do
    query
    |> order_by(asc: :ordering)
  end

  def maybe_exclude_deleted(query, false), do: query
  def maybe_exclude_deleted(query, true) do
    query
    |> where(deleted: false)
  end

  defp changeset(struct, params) do
    struct
    |> cast(params, @change_fields)
    |> foreign_key_constraint(:reference_user_id)
    |> foreign_key_constraint(:reference_bot_id)
  end

  # `update_all` does not set the `updated_at` field so we need to do it
  # ourselves
  defp delete_changes do
    Keyword.put(@delete_changes, :updated_at, DateTime.utc_now())
  end

  defp conflict_set(fields, true), do: fields
  defp conflict_set(fields, false), do: Map.drop(fields, [:ordering])
end
