defmodule WockyAPI.Resolvers.Collection do
  @moduledoc "GraphQL resolver for collection objects"

  alias Wocky.Collections
  alias Wocky.Repo
  alias WockyAPI.Resolvers.Utils

  def get_collection(_root, args, %{context: %{current_user: requestor}}) do
    {:ok,
     args[:id]
     |> Collections.get_query(requestor)
     |> Repo.one()}
  end

  def get_collections(user_or_bot, args, %{context: %{current_user: requestor}}) do
    user_or_bot
    |> Collections.get_collections_query(requestor)
    |> Utils.connection_from_query(user_or_bot, [desc: :created_at], args)
  end

  def get_subscribed_collections(user, args, %{
        context: %{current_user: requestor}
      }) do
    user
    |> Collections.get_subscribed_collections_query(requestor)
    |> Utils.connection_from_query(user, nil, args)
  end

  def get_bots(collection, args, %{context: %{current_user: requestor}}) do
    collection
    |> Collections.get_members_query(requestor)
    |> Utils.connection_from_query(collection, args)
  end

  def get_subscribers(collection, args, %{context: %{current_user: requestor}}) do
    collection
    |> Collections.get_subscribers_query(requestor)
    |> Utils.connection_from_query(collection, args)
  end

  def create(_root, args, %{context: %{current_user: user}}) do
    title = args[:input][:title]

    Collections.create(title, user)
  end

  def update(_root, args, %{context: %{current_user: user}}) do
    id = args[:input][:id]
    title = args[:input][:title]

    Collections.update(id, title, user)
  end

  def delete(_root, args, %{context: %{current_user: user}}) do
    id = args[:id]

    case Collections.delete(id, user) do
      {:ok, _} ->
        {:ok, %{result: true}}

      {:error, _} ->
        {:ok, %{result: false}}
    end
  end

  def subscribe(_root, args, %{context: %{current_user: user}}) do
    id = args[:id]
    Collections.subscribe(id, user)
    {:ok, %{result: true}}
  end

  def unsubscribe(_root, args, %{context: %{current_user: user}}) do
    id = args[:id]
    Collections.unsubscribe(id, user)
    {:ok, %{result: true}}
  end

  def add_bot(_root, args, %{context: %{current_user: user}}) do
    id = args[:id]
    bot_id = args[:bot_id]

    with {:ok, _} <- Collections.add_bot(id, bot_id, user) do
      {:ok, %{result: true}}
    end
  end

  def remove_bot(_root, args, %{context: %{current_user: user}}) do
    id = args[:id]
    bot_id = args[:bot_id]

    with {:ok, _} <- Collections.remove_bot(id, bot_id, user) do
      {:ok, %{result: true}}
    end
  end
end
