defmodule Wocky.Collections do
  @moduledoc "API module for collections"

  import Ecto.Query

  alias Ecto.Queryable
  alias Wocky.Blocking
  alias Wocky.Bot
  alias Wocky.Collections.{Collection, Member, Subscription}
  alias Wocky.Repo
  alias Wocky.User

  @spec get_query(Collection.id(), User.t()) :: Queryable.t()
  def get_query(id, requestor) do
    Collection
    |> identified_by(id)
    |> is_visible_to(requestor.id)
  end

  @spec get_collections_query(User.t() | Bot.t(), User.t()) :: Queryable.t()
  def get_collections_query(%User{id: owner_id}, requestor) do
    Collection
    |> when_owned_by(owner_id)
    |> is_visible_to(requestor.id)
  end

  def get_collections_query(%Bot{id: bot_id}, requestor) do
    Collection
    |> when_has_member(bot_id)
    |> is_visible_to(requestor.id)
  end

  def get_subscribed_collections_query(%User{id: user_id}, requestor) do
    Collection
    |> when_has_subscriber(user_id)
    |> is_visible_to(requestor.id, :id)
  end

  def get_subscribers_query(collection, requestor) do
    collection
    |> Ecto.assoc(:subscribers)
    |> assoc_is_visible_to(requestor.id, :id)
  end

  def get_members_query(collection, requestor) do
    collection
    |> Ecto.assoc(:members)
    |> assoc_is_visible_to(requestor.id)
  end

  @spec create(binary(), User.t()) :: {:ok, Collection.t()}
  def create(title, owner) do
    %Collection{title: title, user_id: owner.id}
    |> Repo.insert(returning: true)
  end

  @spec update(Collection.id(), binary(), User.t())
  :: {:ok, Collection.t()} | {:error, binary()}
  def update(id, title, requestor) do
    with {:ok, coll} <- get_owned_collection(id, requestor.id) do
      coll
      |> Collection.changeset(%{title: title})
      |> Repo.update()
    end
  end

  @spec delete(Collection.id(), User.t())
  :: {:ok, Collection.t() | nil} | {:error, Changeset.t()}
  def delete(id, requestor) do
    with {:ok, coll} <- get_owned_collection(id, requestor.id) do
      Repo.delete(coll)
    else
      {:error, :not_found} ->
        {:ok, nil}
    end
  end

  @spec add_bot(Collection.id(), Bot.id(), User.t())
  :: {:ok, Member.t()} | {:error, Changeset.t() | any()}
  def add_bot(id, bot_id, requestor) do
    with {:ok, _bot} <- get_public_bot(bot_id),
         {:ok, _coll} <- get_owned_collection(id, requestor.id) do
      %Member{}
      |> Member.changeset(%{collection_id: id, bot_id: bot_id})
      |> Repo.insert(on_conflict: :nothing)
    end
  end

  @spec remove_bot(Collection.id(), Bot.id(), User.t())
  :: {:ok, Member.t() | nil} | {:error, any()}
  def remove_bot(id, bot_id, requestor) do
    with {:ok, _coll} <- get_owned_collection(id, requestor.id) do
      result =
        Member
        |> where([m], m.collection_id == ^id and m.bot_id == ^bot_id)
        |> Repo.delete_all(returning: true)

      case result do
        {0, _} -> {:ok, nil}
        {1, [member]} -> {:ok, member}
      end
    end
  end

  @spec subscribe(Collection.id(), User.t())
  :: {:ok, Subscription.t()} | {:error, Changeset.t() | any()}
  def subscribe(id, requestor) do
    with {:ok, _coll} <- get_visible_collection(id, requestor.id) do
      %Subscription{}
      |> Subscription.changeset(%{collection_id: id, user_id: requestor.id})
      |> Repo.insert(on_conflict: :nothing)
    end
  end

  @spec unsubscribe(Collection.id(), User.t())
  :: {:ok, Subscription.t() | nil} | {:error, Changeset.t()}
  def unsubscribe(id, %User{id: user_id} = _requestor) do
    result =
      Subscription
      |> where([s], s.collection_id == ^id and s.user_id == ^user_id)
      |> Repo.delete_all(returning: true)

    case result do
      {0, _} -> {:ok, nil}
      {1, [sub]} -> {:ok, sub}
    end
  end

  defp get_visible_collection(id, user_id) do
    result =
      Collection
      |> identified_by(id)
      |> is_visible_to(user_id)
      |> Repo.one()

    case result do
      nil -> {:error, :not_found}
      coll -> {:ok, coll}
    end
  end

  defp get_owned_collection(id, user_id) do
    result =
      Collection
      |> identified_by(id)
      |> when_owned_by(user_id)
      |> Repo.one()

    case result do
      nil -> {:error, :not_found}
      coll -> {:ok, coll}
    end
  end

  defp get_public_bot(bot_id) do
    case Bot.get(bot_id) do
      nil -> {:error, :bot_not_found}
      bot ->
        if bot.public do
          {:ok, bot}
        else
          {:error, :bot_not_public}
        end
    end
  end

  defp identified_by(q, id) do
    where(q, [c], c.id == ^id)
  end

  defp when_owned_by(q, user_id) do
    where(q, [c], c.user_id == ^user_id)
  end

  defp is_visible_to(q, user_id, field \\ :user_id) do
    Blocking.object_visible_query(q, user_id, field)
  end

  defp assoc_is_visible_to(q, user_id, field \\ :user_id) do
    Blocking.assoc_object_visible_query(q, user_id, field)
  end

  defp when_has_member(q, bot_id) do
    q
    |> join(:inner, [c], b in assoc(c, :members))
    |> where([..., b], b.id == ^bot_id)
  end

  defp when_has_subscriber(q, user_id) do
    q
    |> join(:inner, [c], u in assoc(c, :subscribers))
    |> where([..., u], u.id == ^user_id)
  end
end
