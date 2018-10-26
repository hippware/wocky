defmodule Wocky.WaiterTest do
  use ExUnit.Case, async: true

  import Wocky.Waiter

  describe "wait/3" do
    test "returns :ok if the notify() is called" do
      event = Faker.random_bytes(20)

      Task.start(fn ->
        :timer.sleep(50)
        notify(event)
      end)

      assert wait(event, 200, fn -> false end) == :ok
    end

    test "returns :timeout if notify() is called with a different event" do
      event = Faker.random_bytes(20)

      assert wait(event, 200, fn -> false end) == :timeout
    end

    test "returns :ok if the skip function returns true" do
      event = Faker.random_bytes(20)

      assert wait(event, 200, fn -> true end) == :ok
    end
  end
end
