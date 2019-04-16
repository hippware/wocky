defmodule Wocky.Notifier do
  @moduledoc "Send notifications to the user via several channels"

  @callback notify(struct()) :: :ok

  @known_notifiers [
    Wocky.Notifier.Push,
    Wocky.Notifier.InBand,
    Wocky.Notifier.Email
  ]

  alias Wocky.Block

  @spec notify(struct()) :: :ok
  def notify(event) do
    for notifier <- @known_notifiers do
      type = Module.safe_concat(notifier, Event)
      if type.notify?(event) && deliverable?(event) do
        notifier.notify(event)
      end
    end

    :ok
  end

  defp deliverable?(%{to: to, from: from}), do: !Block.blocked?(to.id, from.id)
  defp deliverable?(_), do: true
end
