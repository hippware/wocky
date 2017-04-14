defmodule Wocky.Events do
  @moduledoc false

  defmodule NewMessageEvent do
    @moduledoc false

    defstruct [:from, :to, :body]

    use ExConstructor
  end

  defmodule BotPerimeterEvent do
    @moduledoc false

    defstruct [:user, :bot, :event]

    use ExConstructor
  end

  defmodule BotShareEvent do
    @moduledoc false

    defstruct [:from, :to, :bot]

    use ExConstructor
  end
end
