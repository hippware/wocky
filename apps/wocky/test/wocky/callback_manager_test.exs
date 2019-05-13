defmodule Wocky.CallbackManagerTest do
  use ExUnit.Case, async: false

  alias Wocky.CallbackManager

  setup do
    CallbackManager.reset(:test)
  end

  test "you can add a callback and get it back" do
    CallbackManager.add(:test, fn -> :success end)

    assert [cb] = CallbackManager.get(:test)
    assert cb.() == :success
  end

  test "you can add multiple callbacks and get them all back" do
    CallbackManager.add(:test, fn -> :first end)
    CallbackManager.add(:test, fn -> :second end)

    assert [cb2, cb1] = CallbackManager.get(:test)
    assert cb1.() == :first
    assert cb2.() == :second
  end

  test "you can directly set the list of callbacks" do
    CallbackManager.set(:test, [fn -> :success end])

    assert [cb] = CallbackManager.get(:test)
    assert cb.() == :success
  end

  test "you can reset the callback list" do
    CallbackManager.add(:test, fn -> :success end)
    CallbackManager.reset(:test)

    assert [] == CallbackManager.get(:test)
  end
end
