defmodule Wocky.HomeStream do
  @moduledoc """
  Context for the home stream.
  """

  import Ecto.Query

  alias Wocky.Bot
  alias Wocky.Collections.Collection
  alias Wocky.HomeStream.{ID, Item, Prepop}
  alias Wocky.JID
  alias Wocky.Repo
  alias Wocky.User

  @doc "Write a home stream item to the database"
  @spec put(User.id(), Item.key(), binary, binary, Keyword.t()) ::
          {:ok, Item.t()} | {:error, term}
  def put(user_id, key, from_jid, stanza, opts \\ []) do
    now = DateTime.utc_now()

    fields = %{
      user_id: user_id,
      key: key,
      from_jid: from_jid,
      stanza: stanza,
      reference_user_id: Keyword.get(opts, :ref_user_id),
      reference_bot_id: Keyword.get(opts, :ref_bot_id),
      reference_bot_item_id: Keyword.get(opts, :ref_bot_item_id),
      reference_collection_id: Keyword.get(opts, :ref_collection_id),
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

    %Item{}
    |> Item.changeset(fields)
    |> Repo.insert(
      on_conflict: [set: on_conflict],
      conflict_target: [:user_id, :key],
      returning: true
    )
  end

  @doc "Mark a single item as deleted"
  @spec delete(User.id(), Item.key()) :: {:ok, Item.t() | nil}
  def delete(user_id, key) when is_binary(key) do
    item = Repo.get_by(Item, %{user_id: user_id, key: key})

    if item != nil do
      item
      |> Item.delete_changeset()
      |> Repo.update()
    else
      {:ok, nil}
    end
  end

  @spec delete(User.t(), User.t() | Bot.t() | Collection.t()) ::
          {:ok, Item.t() | nil}
  def delete(%User{id: user_id}, %User{id: ref_user_id}) do
    Item
    |> where(user_id: ^user_id)
    |> where(reference_user_id: ^ref_user_id)
    |> Repo.update_all(set: Item.delete_changes())

    :ok
  end

  def delete(%User{id: user_id}, %Bot{id: ref_bot_id}) do
    Item
    |> where(user_id: ^user_id)
    |> where(reference_bot_id: ^ref_bot_id)
    |> Repo.update_all(set: Item.delete_changes())

    :ok
  end

  def delete(%User{id: user_id}, %Collection{id: ref_collection_id}) do
    Item
    |> where(user_id: ^user_id)
    |> where(reference_collection_id: ^ref_collection_id)
    |> Repo.update_all(set: Item.delete_changes())

    :ok
  end

  @doc "Get all home stream items for a user"
  @spec get(User.id(), boolean) :: [Item.t()]
  def get(user_id, include_deleted \\ true) do
    user_id
    |> get_query(include_deleted: include_deleted)
    |> order_by(asc: :updated_at)
    |> Repo.all()
  end

  @doc "Get a single item by its key"
  @spec get_by_key(User.id(), Item.key(), boolean) :: Item.t() | nil
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
        ) :: [Item.t()]
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
      Item
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
  def get_query(user_id, opts \\ []) do
    Item
    |> with_user(user_id)
    |> preload(:reference_bot)
    |> maybe_include_ref_update(Keyword.get(opts, :include_ref_updates, false))
    |> maybe_include_deleted(Keyword.get(opts, :include_deleted, false))
  end

  @spec prepopulation_user :: binary
  def prepopulation_user, do: Prepop.handle()

  @spec prepopulate(User.id(), Keyword.t()) :: :ok
  def prepopulate(user_id, opts \\ []) do
    Prepop.prepopulate(user_id, opts)
  end

  @spec update_ref_bot(Bot.t()) :: :ok
  def update_ref_bot(bot) do
    Repo.transaction(fn ->
      Item
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
      key: bot |> ID.bot_changed_id() |> elem(0),
      from_jid: "" |> JID.make(bot.server, "") |> JID.to_binary(),
      class: :ref_update,
      reference_bot_id: bot.id
    }

    %Item{}
    |> Item.changeset(fields)
    |> Repo.insert!(
      on_conflict: :replace_all,
      conflict_target: [:user_id, :key],
      returning: true
    )
  end

  defp with_user(query, user_id) do
    query
    |> where(user_id: ^user_id)
  end

  defp maybe_include_ref_update(query, true), do: query

  defp maybe_include_ref_update(query, false) do
    query
    |> where([i], i.class != ^:ref_update)
  end

  defp maybe_include_deleted(query, true), do: query

  defp maybe_include_deleted(query, false) do
    query
    |> where([i], i.class != ^:deleted)
  end

  defp maybe_limit(query, :none), do: query

  defp maybe_limit(query, limit) do
    query
    |> limit(^limit)
  end
end
