defmodule Wocky.Push.Events do
  @moduledoc false

  alias Wocky.Bot
  alias Wocky.Push.Event
  alias Wocky.User

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

    defp do_get_handle(nil), do: nil
    defp do_get_handle(%User{} = user), do: user.handle

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

    @doc false
    def stringify_id(nil), do: "0"
    def stringify_id(id), do: Integer.to_string(id)
  end

  defmodule BotPerimeterEvent do
    @moduledoc false

    defstruct [:user, :bot, :event]

    @type t :: %__MODULE__{
      user: User.t,
      bot: Bot.t,
      event: :enter | :exit
    }

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

    @type t :: %__MODULE__{
      from: User.t,
      to: User.t,
      bot: Bot.t
    }

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

    defstruct [:from, :to, :body, :image, :conversation_id]

    @type t :: %__MODULE__{
      from: User.t,
      to: User.t,
      body: nil | binary,
      image: nil | binary,
      conversation_id: binary
    }

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

    def uri(%NewMessageEvent{conversation_id: id}) do
      make_uri(:conversation, stringify_id(id))
    end
  end
end
