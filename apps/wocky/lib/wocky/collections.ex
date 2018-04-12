defmodule Wocky.Collections do
  @moduledoc "API module for collections"

  alias Wocky.Collections.Collection
  alias Wocky.Collections.Member
  alias Wocky.Collections.Subscription
  alias Wocky.Bot
  alias Wocky.User

  @spec create(binary(), User.t()) :: {:ok, Collection.t()}
  def create(title, user) do
    Collection.create(title, user)
  end

  @spec update(Collection.id(), binary())
  :: {:ok, Collection.t()} | {:error, Changeset.t()}
  def update(id, title) do
    Collection.update(id, title)
  end

  @spec delete(Collection.id()) :: :ok
  def delete(id) do
    Collection.delete(id)
  end

  @spec add_bot(Collection.id(), Bot.id())
  :: {:ok, Member.t()} | {:error, Changeset.t()}
  def add_bot(id, bot_id) do
    Member.add(id, bot_id)
  end

  @spec remove_bot(Collection.id(), Bot.id())
  :: {:ok, Member.t()} | {:error, Changeset.t()}
  def remove_bot(id, bot_id) do
    Member.remove(id, bot_id)
  end

  @spec subscribe(Collection.id(), User.id())
  :: {:ok, Collection.t()} | {:error, Changeset.t()}
  def subscribe(id, user_id) do
    Subscription.add(id, user_id)
  end

  @spec unsubscribe(Collection.id(), User.id())
  :: {:ok, Collection.t()} | {:error, Changeset.t()}
  def unsubscribe(id, user_id) do
    Subscription.remove(id, user_id)
  end
end
