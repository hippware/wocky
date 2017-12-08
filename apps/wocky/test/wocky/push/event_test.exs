defmodule Wocky.Push.EventTest do
  use ExUnit.Case

  alias Wocky.Push.Event

  test "Event protocol is implemented for strings" do
    assert Event.message("foobar") == "foobar"
    assert Event.uri("foobar") == ""
  end
end
