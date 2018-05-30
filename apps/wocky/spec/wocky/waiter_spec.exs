# credo:disable-for-this-file Credo.Check.Refactor.PipeChainStart
defmodule Wocky.WaiterSpec do
  use ESpec, async: true

  import Wocky.Waiter

  describe "wait/3" do
    it "should return ok if the notify() is called" do
      event = Faker.random_bytes(20)

      Task.start(fn ->
        :timer.sleep(50)
        notify(event)
      end)

      wait(event, 200, fn -> false end)
      |> should(eq :ok)
    end

    it "should return :timeout if notify() is called with a different event" do
      event = Faker.random_bytes(20)

      wait(event, 200, fn -> false end)
      |> should(eq :timeout)
    end

    it "should return ok if the skip function returns true" do
      event = Faker.random_bytes(20)

      wait(event, 200, fn -> true end)
      |> should(eq :ok)
    end
  end
end
