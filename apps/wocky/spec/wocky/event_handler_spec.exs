defmodule Wocky.EventHandlerSpec do
  use ESpec, async: true

  defmodule TestEvent do
    defstruct [:data]
  end

  defmodule TestEventHandler do
    use GenStage

    def start_link(pid), do: GenStage.start_link(__MODULE__, pid)

    def init(pid), do: {:consumer, pid, subscribe_to: [EventHandler]}

    def handle_events(events, _from, pid) do
      _ =
        events
        |> Task.async_stream(fn(e) -> handle_event(e, pid) end)
        |> Enum.to_list

      {:noreply, [], pid}
    end

    defp handle_event(%TestEvent{data: data}, pid), do: send(pid, data)
    defp handle_event(_, _), do: :ok
  end

  describe "broadcast/1" do
    it "should dispatch to event handlers" do
      {:ok, _pid} = TestEventHandler.start_link(self())
      Wocky.EventHandler.broadcast(%TestEvent{data: :test})
      assert_receive(:test, 200)
    end
  end
end
