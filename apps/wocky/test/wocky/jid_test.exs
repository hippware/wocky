defmodule Wocky.JIDTest do
  use ExUnit.Case, async: true
  use Wocky.JID

  test "make/3" do
    assert JID.make("a", "b", "c") == {:jid, "a", "b", "c", "a", "b", "c"}

    assert JID.make(<<0>>, "b", "c") == :error
    assert JID.make("a", "b", <<0>>) == :error
  end

  test "equal?/2" do
    jid = JID.make("a", "b", "c")

    assert JID.equal?(jid, jid)
    refute JID.equal?(jid, JID.make("a", "c"))
  end

  test "from_binary/1" do
    assert JID.from_binary("b") == JID.make("b", "")
    assert JID.from_binary("a@b") == JID.make("a", "b")
    assert JID.from_binary("a@b/c") == JID.make("a", "b", "c")
  end

  test "to_binary/1" do
    assert JID.to_binary(JID.make("a", "b", "c")) == "a@b/c"
  end

  test "to_bare/1" do
    assert JID.to_bare(JID.make("a", "b", "c")) == JID.make("a", "b", "")
  end

  test "replace_resource/2" do
    assert JID.replace_resource(JID.make("a", "b", "c"), "new_resource") ==
             JID.make("a", "b", "new_resource")
  end
end
