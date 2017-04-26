defmodule Wocky.EventHandler.HomeStream do
  @moduledoc ""

  use GenStage
  use Wocky.JID

  import Record, only: [defrecordp: 2, extract: 2]

  alias Wocky.Bot
  alias Wocky.Events.BotPerimeterEvent
  alias Wocky.User

  require Record

  defrecordp :xmlel, extract(:xmlel, from_lib: "exml/include/exml.hrl")
  defrecordp :xmlcdata, extract(:xmlcdata, from_lib: "exml/include/exml.hrl")

  @spec start_link :: {:ok, pid}
  def start_link do
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
    owner_jid = JID.from_binary(bot.owner)

    :ejabberd_router.route(JID.make("", :wocky_app.server),
                           JID.to_bare(owner_jid),
                           format(event))
  end
  defp handle_event(_, _) do
    :ok
  end

  defp format(%BotPerimeterEvent{user: user, bot: bot, event: event}) do
    user_jid_str = User.to_bare_jid_string(user)
    bot_jid_str = Bot.to_jid_string(bot)
    xmlel(name: "message",
          attrs: [
            {"from", :wocky_app.server},
            {"to", bot.owner},
            {"type", "headline"}
          ],
          children: [
            xmlel(name: "bot", attrs: [{"xmlns", "hippware.com/hxep/bot"}],
                  children: [
                    xmlel(name: "jid", children: [
                            xmlcdata(content: bot_jid_str)
                          ]),
                    xmlel(name: "id", children: [
                            xmlcdata(content: bot.id)
                          ]),
                    xmlel(name: "server", children: [
                            xmlcdata(content: bot.server)
                          ]),
                    xmlel(name: "action", children: [
                            xmlcdata(content: to_string(event))
                          ]),
                    xmlel(name: "user-jid", children: [
                            xmlcdata(content: user_jid_str)
                          ])
                  ])
          ])
  end
  defp format(event) do
    inspect(event)
  end
end
