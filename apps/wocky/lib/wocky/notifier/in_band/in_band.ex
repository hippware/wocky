defmodule Wocky.Notifier.InBand do
  @moduledoc "Implementation of in-band notifications"

  @behaviour Wocky.Notifier

  alias Wocky.Notifier.InBand.Event
  alias Wocky.Notifier.InBand.Notification

  @impl true
  def notify(event) do
    {:ok, _} =
      Notification.put(
        Event.transform(event),
        Event.event_type(event),
        Event.required_fields(event)
      )

    :ok
  end
end
