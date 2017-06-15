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

  @spec start_link :: {:ok, pid}
  def start_link do
    :ok = PushNotifier.init
    GenStage.start_link(__MODULE__, [], name: :push_notification_event_handler)
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
    if Application.get_env(:wocky, :async_push_notifications, true) do
      _ =
        events
        |> Task.async_stream(fn e -> handle_event(e, state) end)
        |> Enum.to_list
    else
      Enum.each events, fn e -> handle_event(e, state) end
    end

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
    PushNotifier.push_all(to_jid(to), body)
  end
  defp handle_event(%NewMessageEvent{to: to} = event, _state) do
    body = format(event)
    PushNotifier.push_all(to_jid(to), body)
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

  defp to_jid(%User{} = user), do: User.to_jid(user)
  defp to_jid(jid() = jid), do: jid

  defp get_handle(obj) do
    case do_get_handle(obj) do
      nil -> "Someone"
      "" -> "Someone"
      handle -> handle
    end
  end

  defp do_get_handle(obj) do
    case get_user(obj) do
      nil -> nil
      user -> user.handle
    end
  end

  defp get_user(nil), do: nil
  defp get_user(%User{} = user), do: user
  defp get_user(jid() = jid), do: User.get_by_jid(jid)
end
