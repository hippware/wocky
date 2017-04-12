defmodule Wocky.EventHandler.PushNotification do
  @moduledoc """
  """

  use GenStage

  alias Wocky.Events.BotPerimeterEvent
  alias Wocky.JID
  alias Wocky.PushNotifier

  @spec start_link(Keyword.t) :: {:ok, pid}
  def start_link(cfg_terms) do
    :ok = PushNotifier.init(cfg_terms)
    GenStage.start_link(__MODULE__, [])
  end

  ## Callbacks

  @spec init(list) :: {:consumer, any, Keyword.t}
  def init([]) do
    # Starts a permanent subscription to the broadcaster
    # which will automatically start requesting items.
    {:consumer, [], subscribe_to: [EventHandler]}
  end

  @spec handle_events(list, term, module) :: {:noreply, [], any}
  def handle_events(events, _from, state) do
    _ =
      events
      |> Task.async_stream(fn(e) -> handle_event(e, state) end)
      |> Enum.to_list

    {:noreply, [], state}
  end

  defp handle_event(%BotPerimeterEvent{bot: bot} = event, _state) do
    body = format(event)

    bot.owner
    |> JID.from_binary
    |> PushNotifier.push(body)
  end
  defp handle_event(_, _) do
    :ok
  end

  defp format(%BotPerimeterEvent{user: user, bot: bot, event: event}) do
    case event do
      :enter -> "#{user.handle} is near the bot #{bot.title}"
      :exit -> "#{user.handle} is leaving the area for #{bot.title}"
    end
  end
  defp format(event) do
    inspect(event)
  end
end
