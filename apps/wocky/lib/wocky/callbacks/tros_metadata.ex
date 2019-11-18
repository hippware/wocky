defmodule Wocky.Callbacks.TROSMetadata do
  @moduledoc """
  Callbacks for TROS metadata changes
  """

  use DawdleDB.Handler, type: Wocky.TROS.Metadata

  alias Wocky.TROS
  alias Wocky.Waiter

  @impl true
  def handle_update(%{ready: true, id: id}, %{ready: false}) do
    id
    |> TROS.file_ready_event()
    |> Waiter.notify()
  end

  def handle_update(_new, _old), do: :ok
end
