defmodule Wocky.Push.Events do
  @moduledoc false

  alias Wocky.Push.Event

  defmodule Utils do
    @moduledoc false

    use Wocky.JID

    alias Wocky.User

    @doc false
    def blank?(nil), do: true
    def blank?(""), do: true
    def blank?(_), do: false

    @doc false
    def get_handle(obj) do
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

    @doc false
    def make_uri(type, id) do
      "#{uri_prefix()}://#{type}/#{server()}/#{id}"
    end

    defp server do
      Confex.get_env(:wocky, :wocky_host)
    end

    defp uri_prefix do
      Confex.get_env(:wocky, Wocky.Push)[:uri_prefix]
    end
  end

  defmodule BotPerimeterEvent do
    @moduledoc false

    defstruct [:user, :bot, :event]

    use ExConstructor
  end

  defimpl Event, for: BotPerimeterEvent do
    import Wocky.Push.Events.Utils
    def message(%BotPerimeterEvent{user: user, bot: bot, event: event}) do
      case event do
        :enter -> "#{user.handle} is near the bot #{bot.title}"
        :exit -> "#{user.handle} is leaving the area for #{bot.title}"
      end
    end

    def uri(%BotPerimeterEvent{bot: bot}), do: make_uri(:bot, bot.id)
  end

  defmodule BotShareEvent do
    @moduledoc false

    defstruct [:from, :to, :bot]

    use ExConstructor
  end

  defimpl Event, for: BotShareEvent do
    import Wocky.Push.Events.Utils
    def message(%BotShareEvent{from: from}) do
      get_handle(from) <> " shared a bot with you!"
    end

    def uri(%BotShareEvent{bot: bot}), do: make_uri(:bot, bot.id)
  end

  defmodule NewMessageEvent do
    @moduledoc false

    defstruct [:from, :to, :body, :image]

    use ExConstructor
  end

  defimpl Event, for: NewMessageEvent do
    import Wocky.Push.Events.Utils
    def message(%NewMessageEvent{from: from, body: body}) do
      if blank?(body) do
        get_handle(from) <> " sent you an image!"
      else
        "From: #{get_handle(from)}\n#{body}"
      end
    end

    def uri(%NewMessageEvent{} = _event), do: make_uri(:conversation, "NONE")
  end
end
