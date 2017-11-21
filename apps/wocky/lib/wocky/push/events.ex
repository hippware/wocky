defmodule Wocky.Push.Events do
  @moduledoc false

  alias Wocky.Push.Event

  defmodule Utils do
    @moduledoc false

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
  end

  defmodule BotPerimeterEvent do
    @moduledoc false

    defstruct [:user, :bot, :event]

    use ExConstructor
  end

  defimpl Event, for: BotPerimeterEvent do
    def format(%BotPerimeterEvent{user: user, bot: bot, event: event}) do
      case event do
        :enter -> "#{user.handle} is near the bot #{bot.title}"
        :exit -> "#{user.handle} is leaving the area for #{bot.title}"
      end
    end
  end

  defmodule BotShareEvent do
    @moduledoc false

    defstruct [:from, :to, :bot]

    use ExConstructor
  end

  defimpl Event, for: BotShareEvent do
    import Wocky.Push.Events.Utils
    def format(%BotShareEvent{from: from}) do
      get_handle(from) <> " shared a bot with you!"
    end
  end

  defmodule NewMessageEvent do
    @moduledoc false

    defstruct [:from, :to, :body, :image]

    use ExConstructor
  end

  defimpl Event, for: NewMessageEvent do
    import Wocky.Push.Events.Utils
    def format(%NewMessageEvent{from: from, body: body}) do
      if blank?(body) do
        get_handle(from) <> " sent you an image!"
      else
        "From: #{get_handle(from)}\n#{body}"
      end
    end
  end
end
