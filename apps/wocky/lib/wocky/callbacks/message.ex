defmodule Wocky.Callbacks.Message do
  @moduledoc """
  Callbacks for DB message changes
  """

  use DawdleDB.Handler, type: Wocky.Messaging.Message

  alias Wocky.Events.NewMessage
  alias Wocky.Notifier
  alias Wocky.Repo.Hydrator

  def handle_insert(new) do
    Hydrator.with_assocs(new, [:sender, :recipient], &send_push(&1))
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
