defmodule Wocky.Callbacks.TROSMetadata do
  @moduledoc """
  Callbacks for TROS metadata changes
  """

  alias Wocky.{TROS, Waiter}

  use Wocky.Watcher, type: Wocky.TROS.Metadata, events: [:update]

  def handle_update(%Event{old: %{ready: false}, new: %{ready: true, id: id}}) do
    id
    |> TROS.file_ready_event()
    |> Waiter.notify()
  end
end
