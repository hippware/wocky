defmodule Wocky.AuditTest do
  use Wocky.DataCase, async: false

  alias Timex.Duration
  alias Wocky.Audit
  alias Wocky.Audit.TrafficLog
  alias Wocky.Repo.Factory
  alias Wocky.Repo.ID

  @packets 100

  setup do
    {:ok, user: Factory.insert(:user)}
  end

  describe "log_traffic/3" do
    test "should add a traffic entry for the user", %{user: user} do
      now = Timex.now()
      log = Factory.params_for(:traffic_log, user_id: user.id)

      result = Audit.log_traffic(log, user, log_traffic: true)
      assert {:ok, _} = result

      entry = Kernel.elem(result, 1)
      assert %{__struct__: TrafficLog} = entry

      assert Audit.get_traffic_by_period(user.id, now, default_duration()) ==
               [entry]
    end

    test "should not log when :log_traffic is false", %{user: user} do
      now = Timex.now()
      log = Factory.params_for(:traffic_log, user_id: user.id)

      assert {:ok, nil} = Audit.log_traffic(log, user, log_traffic: false)

      assert Audit.get_traffic_by_period(user.id, now, default_duration()) ==
               []
    end
  end

  defp traffic_fixture(%{user: user}) do
    device = ID.new()

    traffic =
      for _ <- 1..@packets do
        Factory.insert(:traffic_log, user: user, device: device)
      end

    Factory.insert(:traffic_log, user: user, device: ID.new())

    {:ok,
     traffic: traffic,
     device: device,
     first: hd(traffic).created_at,
     middle: Enum.at(traffic, div(@packets, 2) - 1).created_at,
     last: List.last(traffic).created_at}
  end

  describe "get_traffic_by_period/3" do
    setup :traffic_fixture

    test "should get all traffic from the specified time/duration", ctx do
      packets =
        Audit.get_traffic_by_period(
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

      packets! = Audit.get_traffic_by_period(ctx.user.id, ctx.first, dur)
      assert length(packets!) == div(@packets, 2)

      packets! =
        Audit.get_traffic_by_period(
          ctx.user.id,
          ctx.last,
          Duration.from_microseconds(0)
        )

      assert length(packets!) == 1
    end

    test "should get no traffic for a non-existant user" do
      packets =
        Audit.get_traffic_by_period(
          ID.new(),
          default_start(),
          default_duration()
        )

      assert packets == []
    end
  end

  describe "get_traffic_by_device/4" do
    setup :traffic_fixture

    test "should get all traffic from the specified device/time/duration",
         ctx do
      packets =
        Audit.get_traffic_by_device(
          ctx.user.id,
          ctx.device,
          default_start(),
          default_duration()
        )

      assert length(packets) == @packets
    end

    test "should get nothing from a different device", ctx do
      packets =
        Audit.get_traffic_by_device(
          ctx.user.id,
          "fnord",
          default_start(),
          default_duration()
        )

      assert packets == []
    end

    test "should get no traffic for a non-existant user", ctx do
      packets =
        Audit.get_traffic_by_device(
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
