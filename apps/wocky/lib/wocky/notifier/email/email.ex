defmodule Wocky.Notifier.Email do
  @moduledoc false

  @behaviour Wocky.Notifier

  alias Wocky.Notifier.Email.Event

  @impl true
  def notify(event) do
    Event.send(event)
  end
end
