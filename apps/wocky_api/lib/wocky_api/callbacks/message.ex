defmodule WockyAPI.Callbacks.Message do
  @moduledoc """
  Callbacks for DB message changes
  """

  alias Wocky.Repo
  alias WockyAPI.Resolvers.Message, as: MessageResolver

  use Wocky.Watcher, type: Wocky.Message, events: [:insert]

  def handle_insert(%Event{new: new}) do
    new = Repo.preload(new, [:sender])
    if not is_nil(new.sender), do: MessageResolver.notify_message(new)
  end
end
