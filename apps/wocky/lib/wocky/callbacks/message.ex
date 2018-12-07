defmodule Wocky.Callbacks.Message do
  @moduledoc """
  Callbacks for DB message changes
  """

  use Wocky.Watcher, type: Wocky.Message, events: [:insert]

  alias Wocky.Push
  alias Wocky.Push.Events.NewMessageEvent
  alias Wocky.Repo

  def handle_insert(%Event{action: :insert, new: new}) do
    new = Repo.preload(new, [:sender, :recipient])

    if new.sender != nil && new.recipient != nil,
      do: send_push(new)
  end

  defp send_push(msg) do
    event = %NewMessageEvent{
      to: msg.recipient,
      from: msg.sender,
      content: msg.content,
      image_url: msg.image_url,
      conversation_id: msg.id
    }

    Push.notify_all(msg.recipient, event)
  end
end
