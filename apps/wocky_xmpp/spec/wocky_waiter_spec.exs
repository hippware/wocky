defmodule :wocky_waiter_spec do
  use ESpec, async: true

  import :wocky_waiter

  alias Faker.Lorem

  describe "wait/3" do
    it "should return ok if the notify() is called" do
      event = Lorem.characters(20)

      Task.start(fn ->
        :timer.sleep(50)
        notify(event)
      end)

      wait(event, 200, fn -> false end)
      |> should(eq :ok)
    end

    it "should return :timeout if notify() is called with a different event" do
      event = Lorem.characters(20)

      wait(event, 200, fn -> false end)
      |> should(eq :timeout)
    end

    it "should return ok if the skip function returns true" do
      event = Lorem.characters(20)

      wait(event, 200, fn -> true end)
      |> should(eq :ok)
    end
  end
end
