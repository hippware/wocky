defmodule WockyAPI.Callbacks.Message do
  @moduledoc """
  Callbacks for DB message changes
  """

  use DawdleDB.Handler, type: Wocky.Message

  alias Wocky.Repo
  alias WockyAPI.Resolvers.Message, as: MessageResolver

  def handle_insert(new) do
    new = Repo.preload(new, [:sender])
    if not is_nil(new.sender), do: MessageResolver.notify_message(new)
  end
end
