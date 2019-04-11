defmodule Wocky.Notifier do
  @moduledoc "Send notifications to the user via several channels"

  @callback notify(struct()) :: :ok

  @known_notifiers [
    {Wocky.Notifier.Push.Event, Wocky.Notifier.Push},
    {Wocky.Notifier.InBand.Event, Wocky.Notifier.InBand},
    {Wocky.Notifier.Mailer.Event, Wocky.Notifier.Mailer}
  ]

  def notify(event) do
    # TODO Handle blocking (where appropriate)
    # if !Block.blocked?(notification.user_id, notification.other_user_id) do
    for {type, notifier} <- @known_notifiers do
      if type.notify?(event) do
        notifier.notify(event)
      end
    end

    # end
  end
end
