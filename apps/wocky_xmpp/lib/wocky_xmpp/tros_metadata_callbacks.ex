defmodule WockyXMPP.TROSMetadataCallbacks do
  @moduledoc """
  Callbacks for DB bot changes
  """
  alias Wocky.TROS.Metadata
  alias Wocky.Watcher.Client
  alias WockyDBWatcher.Event

  def register do
    Client.subscribe(Metadata, :update, &handle_update/1)
  end

  def handle_update(%Event{
        old: %Metadata{ready: false},
        new: %Metadata{id: id, ready: true}
      }) do
    id
    |> :mod_wocky_tros.waiter_event()
    |> :wocky_waiter.notify()
  end

  def handle_update(_), do: :ok
end
