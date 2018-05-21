defmodule WockyAPI.Callbacks.HomeStreamItem do
  @moduledoc """
  Callbacks for Home Stream Item changes
  """
  alias Wocky.HomeStream.Item
  alias Wocky.Watcher.Client
  alias WockyAPI.Resolvers.User, as: UserResolver
  alias WockyDBWatcher.Event

  def register do
    Client.subscribe(Item, :insert, &handle_insert/1)
    Client.subscribe(Item, :update, &handle_update/1)
  end

  def handle_insert(%Event{new: item}) do
    UserResolver.notify_home_stream(item, :insert)
  end

  def handle_update(%Event{
        old: %Item{class: :deleted},
        new: %Item{class: :deleted}
      }) do
    :ok
  end

  def handle_update(%Event{new: %Item{class: :deleted} = item}) do
    UserResolver.notify_home_stream(item, :delete)
  end

  def handle_update(%Event{new: item}) do
    UserResolver.notify_home_stream(item, :update)
  end
end
