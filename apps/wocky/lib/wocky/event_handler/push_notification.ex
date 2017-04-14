defmodule Wocky.EventHandler.PushNotification do
  @moduledoc """
  """

  use GenStage
  use Wocky.JID

  alias Wocky.Events.BotPerimeterEvent
  alias Wocky.Events.BotShareEvent
  alias Wocky.Events.NewMessageEvent
  alias Wocky.PushNotifier
  alias Wocky.User

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
  defp handle_event(%BotShareEvent{to: to} = event, _state) do
    body = format(event)
    PushNotifier.push_all(to, body)
  end
  defp handle_event(%NewMessageEvent{to: to} = event, _state) do
    body = format(event)
    PushNotifier.push_all(to, body)
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
  defp format(%BotShareEvent{from: from}) do
    get_handle(from) <> " shared a bot with you!"
  end
  defp format(%NewMessageEvent{from: from, body: body}) do
    if is_nil(from) do
      body
    else
      "From: #{get_handle(from)}\n#{body}"
    end
  end
  defp format(event) do
    inspect(event)
  end

  defp get_handle(from) do
    jid(luser: user) = from
    case User.get_handle(user) do
      nil -> "Someone"
      h -> h
    end
  end
end
