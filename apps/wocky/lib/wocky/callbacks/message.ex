defmodule Wocky.Callbacks.Message do
  @moduledoc """
  Callbacks for DB message changes
  """

  use DawdleDB.Handler, type: Wocky.Messaging.Message

  alias Wocky.Events.NewMessage
  alias Wocky.Notifier
  alias Wocky.Repo

  def handle_insert(new) do
    new = Repo.preload(new, [:sender, :recipient])

    if new.sender != nil && new.recipient != nil,
      do: send_push(new)
  end

  defp send_push(msg) do
    %NewMessage{
      to: msg.recipient,
      from: msg.sender,
      content: msg.content,
      image_url: msg.image_url,
      conversation_id: msg.id
    }
    |> Notifier.notify()
  end
end
