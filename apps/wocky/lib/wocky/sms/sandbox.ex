defmodule Wocky.SMS.Sandbox do
  @moduledoc "Implements the SMS Messenger behavior in a testing sandbox"

  @behaviour Wocky.SMS.Messenger

  def send(_recipient, _body) do
    :ok
  end
end
