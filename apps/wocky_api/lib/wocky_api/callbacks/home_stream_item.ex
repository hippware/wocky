defmodule WockyAPI.Callbacks.HomeStreamItem do
  @moduledoc """
  Callbacks for Home Stream Item changes
  """

  use Wocky.Watcher, type: Wocky.HomeStream.Item, events: [:insert, :update]

  alias Wocky.HomeStream.Item
  alias WockyAPI.Resolvers.User, as: UserResolver

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
