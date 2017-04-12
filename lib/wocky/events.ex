defmodule Wocky.Events do
  @moduledoc false

  defmodule BotPerimeterEvent do
    @moduledoc false

    defstruct [:user, :bot, :event]
  end
end
