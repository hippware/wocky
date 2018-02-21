defmodule Wocky.HomeStreamItem do
  @moduledoc """
  DB interface module for home stream items
  """

  use Wocky.Repo.Schema

  import Ecto.Query
  import EctoHomoiconicEnum, only: [defenum: 2]

  alias Timex.Duration
  alias Wocky.Bot
  alias Wocky.Bot.Share
  alias Wocky.HomeStreamID
  alias Wocky.JID
  alias Wocky.Repo
  alias Wocky.User
  alias __MODULE__

  defenum ClassEnum, [:item, :deleted, :ref_update]

  @foreign_key_type :binary_id
  schema "home_stream_items" do
    field :key, :string
    field :from_jid, :binary, default: ""
    field :stanza, :binary, default: ""
    field :class, ClassEnum, default: :item

    belongs_to :user, User
    belongs_to :reference_user, User, foreign_key: :reference_user_id
    belongs_to :reference_bot, Bot, foreign_key: :reference_bot_id

    timestamps()
  end

  @type key :: binary
  @type class :: :item | :deleted | :ref_update

  @type t :: %HomeStreamItem{
          user_id: User.id(),
          key: key,
          from_jid: binary,
          stanza: binary,
          class: class,
          updated_at: DateTime.t(),
          reference_user: User.t(),
          reference_bot: Bot.t()
        }

  @change_fields [
    :user_id,
    :key,
    :from_jid,
    :stanza,
    :class,
    :reference_user_id,
    :reference_bot_id,
    :created_at,
    :updated_at
  ]

  @prepopulate_fields @change_fields

  @delete_changes [
    class: :deleted,
    stanza: "",
    from_jid: "",
    reference_user_id: nil,
    reference_bot_id: nil
  ]

  @doc "Write a home stream item to the database"
  @spec put(User.id(), key, binary, binary, Keyword.t()) ::
          {:ok, t} | {:error, term}
  def put(user_id, key, from_jid, stanza, opts \\ []) do
    now = DateTime.utc_now()

    fields = %{
      user_id: user_id,
      key: key,
      from_jid: from_jid,
      stanza: stanza,
      reference_user_id: Keyword.get(opts, :ref_user_id),
      reference_bot_id: Keyword.get(opts, :ref_bot_id),
      class: :item,

      # Usually we let ecto handle these timestamps, however in this case we
      # want to know if the item is newly inserted. We do this by checking if
      # the two values match which, if we let ecto handle it, they never
      # actually do (since, I presume, it makes two internal now() calls).
      updated_at: now,
      created_at: now
    }

    on_conflict =
      fields
      |> Map.drop([:created_at])
      |> Map.to_list()

    %HomeStreamItem{}
    |> changeset(fields)
    |> Repo.insert(
      on_conflict: [set: on_conflict],
      conflict_target: [:user_id, :key],
      returning: true
    )
  end

  @doc "Mark a single item as deleted"
  @spec delete(User.id(), key) :: {:ok, t | nil}
  def delete(user_id, key) when is_binary(key) do
    item = Repo.get_by(HomeStreamItem, %{user_id: user_id, key: key})

    if item != nil do
      item
      |> changeset(Map.new(@delete_changes))
      |> Repo.update()
    else
      {:ok, nil}
    end
  end

  @spec delete(User.t(), User.t() | Bot.t()) :: {:ok, t | nil}
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

  @spec delete_by_user_ref(User.t()) :: :ok
  def delete_by_user_ref(user) do
    HomeStreamItem
    |> where(reference_user_id: ^user.id)
    |> Repo.update_all(set: delete_changes())

    :ok
  end

  @spec delete_by_bot_ref(Bot.t()) :: :ok
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
  @spec delete_by_bot_ref_invisible(Bot.t()) :: :ok
  def delete_by_bot_ref_invisible(%Bot{public: true}), do: :ok

  def delete_by_bot_ref_invisible(bot) do
    Repo.transaction(fn ->
      HomeStreamItem
      |> join(
        :left,
        [i],
        s in Share,
        s.user_id == i.user_id and s.bot_id == ^bot.id
      )
      |> where(
        [i, s],
        i.reference_bot_id == ^bot.id and is_nil(s.user_id) and
          i.user_id != ^bot.user_id
      )
      |> Repo.stream()
      |> Stream.each(fn item ->
        item
        |> changeset(Map.new(@delete_changes))
        |> Repo.update()
      end)
      |> Stream.run()
    end)

    :ok
  end

  @doc "Get all home stream items for a user"
  @spec get(User.id(), boolean) :: [t]
  def get(user_id, include_deleted \\ true) do
    user_id
    |> get_query(include_deleted: include_deleted)
    |> order_by(asc: :updated_at)
    |> Repo.all()
  end

  @doc "Get a single item by its key"
  @spec get_by_key(User.id(), key, boolean) :: t | nil
  def get_by_key(user_id, key, include_deleted \\ true) do
    user_id
    |> get_query(include_deleted: include_deleted)
    |> where(key: ^key)
    |> Repo.one()
  end

  @doc """
  Get all items after a certain timestamp *including* ref_update items
  """
  @spec get_after_time(
          User.id(),
          DateTime.t(),
          non_neg_integer | :none,
          boolean
        ) :: [t]
  def get_after_time(user_id, time, limit \\ :none, include_deleted \\ true) do
    user_id
    |> get_query(include_deleted: include_deleted, include_ref_updates: true)
    |> where([i], i.updated_at > ^time)
    |> order_by(asc: :updated_at)
    |> maybe_limit(limit)
    |> Repo.all()
  end

  @doc "Get the latest version for a user"
  @spec get_latest_version(User.id()) :: DateTime.t()
  def get_latest_version(user_id) do
    time =
      HomeStreamItem
      |> with_user(user_id)
      |> select([i], max(i.updated_at))
      |> Repo.one()

    if time == nil do
      DateTime.from_unix!(0)
    else
      time
    end
  end

  @spec get_query(User.id(), Keyword.t()) :: Ecto.Queryable.t()
  def get_query(user_id, opts) do
    HomeStreamItem
    |> with_user(user_id)
    |> preload(:reference_bot)
    |> maybe_include_ref_update(Keyword.get(opts, :include_ref_updates, false))
    |> maybe_include_deleted(Keyword.get(opts, :include_deleted, false))
  end

  @spec prepopulate_from(User.id(), User.id(), Duration.t(), non_neg_integer) ::
          :ok
  def prepopulate_from(user_id, from_id, period, min) do
    from_id
    |> prepop_items(period, min)
    |> Enum.each(fn i ->
      params =
        i
        |> Map.take(@prepopulate_fields)
        |> Map.put(:user_id, user_id)

      %HomeStreamItem{}
      |> cast(params, @prepopulate_fields)
      |> foreign_key_constraint(:reference_user_id)
      |> foreign_key_constraint(:reference_bot_id)
      |> Repo.insert(
        on_conflict: :replace_all,
        conflict_target: [:user_id, :key]
      )
    end)
  end

  @spec update_ref_bot(Bot.t()) :: :ok
  def update_ref_bot(bot) do
    Repo.transaction(fn ->
      HomeStreamItem
      |> where(reference_bot_id: ^bot.id)
      |> where(class: ^:item)
      |> distinct([i], [i.user_id])
      |> preload(:user)
      |> Repo.stream()
      |> Stream.map(&update_ref_bot(bot, &1.user))
      |> Stream.run()
    end)
  end

  defp update_ref_bot(bot, user) do
    fields = %{
      user_id: user.id,
      key: bot |> HomeStreamID.bot_changed_id() |> elem(0),
      from_jid: "" |> JID.make(bot.server, "") |> JID.to_binary(),
      class: :ref_update,
      reference_bot_id: bot.id
    }

    %HomeStreamItem{}
    |> changeset(fields)
    |> Repo.insert!(
      on_conflict: :replace_all,
      conflict_target: [:user_id, :key],
      returning: true
    )
  end

  defp prepop_items(from_id, period, min) do
    from_time = Timex.subtract(DateTime.utc_now(), period)

    time_items = get_after_time(from_id, from_time, :none, true)

    if length(time_items) < min do
      get_by_count(from_id, min)
    else
      time_items
    end
  end

  defp get_by_count(user_id, count) do
    user_id
    |> get_query(include_deleted: true)
    |> order_by(desc: :updated_at)
    |> limit(^count)
    |> Repo.all()
  end

  def with_user(user_id), do: with_user(HomeStreamItem, user_id)

  def with_user(query, user_id) do
    query
    |> where(user_id: ^user_id)
  end

  def maybe_include_ref_update(query, true), do: query

  def maybe_include_ref_update(query, false) do
    query
    |> where([i], i.class != ^:ref_update)
  end

  def maybe_include_deleted(query, true), do: query

  def maybe_include_deleted(query, false) do
    query
    |> where([i], i.class != ^:deleted)
  end

  def maybe_limit(query, :none), do: query

  def maybe_limit(query, limit) do
    query
    |> limit(^limit)
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
end
