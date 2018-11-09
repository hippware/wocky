defmodule Wocky.TrafficLogTest do
  use Wocky.DataCase, async: false

  alias Timex.Duration
  alias Wocky.Repo.Factory
  alias Wocky.Repo.ID
  alias Wocky.TrafficLog

  @packets 100

  setup do
    user = Factory.insert(:user)
    device = ID.new()

    traffic =
      for _ <- 1..@packets do
        Factory.insert(:traffic_log, user: user, device: device)
      end

    Factory.insert(:traffic_log, user: user, device: ID.new())

    {:ok,
     user: user,
     traffic: traffic,
     device: device,
     first: hd(traffic).created_at,
     middle: Enum.at(traffic, div(@packets, 2) - 1).created_at,
     last: List.last(traffic).created_at}
  end

  describe "put/1" do
    setup do
      {:ok, user: Factory.insert(:user)}

      on_exit(fn -> Application.put_env(:wocky, :log_traffic, true) end)
    end

    test "should add a traffic entry for the user", %{user: user} do
      now = Timex.now()
      log = Factory.params_for(:traffic_log, user_id: user.id)

      put_result = TrafficLog.put(log)
      assert {:ok, _} = put_result

      entry = Kernel.elem(put_result, 1)
      assert %{__struct__: TrafficLog} = entry

      assert TrafficLog.get_by_period(user.id, now, default_duration()) == [
               entry
             ]
    end

    test "should not log when :log_traffic is false", %{user: user} do
      Application.put_env(:wocky, :log_traffic, false)

      now = Timex.now()
      log = Factory.params_for(:traffic_log, user_id: user.id)

      assert {:ok, nil} = TrafficLog.put(log)
      assert TrafficLog.get_by_period(user.id, now, default_duration()) == []
    end

    test "`force` param should override log disabling", %{user: user} do
      Application.put_env(:wocky, :log_traffic, false)

      now = Timex.now()
      log = Factory.params_for(:traffic_log, user_id: user.id)

      put_result = TrafficLog.put(log, true)
      assert {:ok, _} = put_result

      entry = Kernel.elem(put_result, 1)
      assert %{__struct__: TrafficLog} = entry

      assert TrafficLog.get_by_period(user.id, now, default_duration()) == [
               entry
             ]
    end
  end

  describe "get_by_period/3" do
    test "should get all traffic from the specified time/duration", ctx do
      packets =
        TrafficLog.get_by_period(
          ctx.user.id,
          default_start(),
          default_duration()
        )

      assert length(packets) == @packets + 1
    end

    test "should get the subset of packets in the time specified", ctx do
      dur =
        ctx.middle
        |> Timex.diff(ctx.first)
        |> Duration.from_microseconds()

      packets = TrafficLog.get_by_period(ctx.user.id, ctx.first, dur)
      assert length(packets) == div(@packets, 2)

      packets =
        TrafficLog.get_by_period(
          ctx.user.id,
          ctx.last,
          Duration.from_microseconds(0)
        )

      assert length(packets) == 1
    end

    test "should get no traffic for a non-existant user" do
      packets =
        TrafficLog.get_by_period(ID.new(), default_start(), default_duration())

      assert packets == []
    end
  end

  describe "get_by_device/4" do
    test "should get all traffic from the specified device/time/duration",
         ctx do
      packets =
        TrafficLog.get_by_device(
          ctx.user.id,
          ctx.device,
          default_start(),
          default_duration()
        )

      assert length(packets) == @packets
    end

    test "should get nothing from a different device", ctx do
      packets =
        TrafficLog.get_by_device(
          ctx.user.id,
          "fnord",
          default_start(),
          default_duration()
        )

      assert packets == []
    end

    test "should get no traffic for a non-existant user", ctx do
      packets =
        TrafficLog.get_by_device(
          ID.new(),
          ctx.device,
          default_start(),
          default_duration()
        )

      assert packets == []
    end
  end

  defp default_start, do: Timex.subtract(Timex.now(), Duration.from_hours(1))

  defp default_duration, do: Duration.from_hours(1)
end
