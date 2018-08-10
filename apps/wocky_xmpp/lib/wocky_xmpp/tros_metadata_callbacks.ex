defmodule WockyXMPP.TROSMetadataCallbacks do
  @moduledoc """
  Callbacks for DB bot changes
  """

  use Elixometer
  use Wocky.Watcher, type: Wocky.TROS.Metadata, events: [:update]

  alias Wocky.TROS.Metadata
  alias Wocky.Waiter

  def handle_update(%Event{
        old: %Metadata{ready: false},
        new: %Metadata{id: id, ready: true}
      }) do
    update_counter("tros.upload.complete", 1)

    id
    |> :mod_wocky_tros.waiter_event()
    |> Waiter.notify()
  end

  def handle_update(_), do: :ok
end
