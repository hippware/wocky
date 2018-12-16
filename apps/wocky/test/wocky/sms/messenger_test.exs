defmodule Wocky.SMS.MessengerTest do
  use ExUnit.Case

  alias Wocky.SMS.Messenger

  # This is just a bare bones sanity check at this point.

  test "send/2" do
    assert :ok == Messenger.send("+15551234567", "testing")
  end
end
