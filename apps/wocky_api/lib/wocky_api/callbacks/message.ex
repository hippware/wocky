defmodule WockyAPI.Callbacks.Message do
  @moduledoc """
  Callbacks for DB message changes
  """

  use DawdleDB.Handler, type: Wocky.Messaging.Message

  alias Wocky.Repo.Hydrator
  alias WockyAPI.Resolvers.Message, as: MessageResolver

  def handle_insert(new) do
    Hydrator.with_assocs(new, [:sender], fn rec ->
      MessageResolver.notify_message(rec)
    end)
  end
end
