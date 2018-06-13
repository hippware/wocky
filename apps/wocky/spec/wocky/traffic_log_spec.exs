# credo:disable-for-this-file Credo.Check.Refactor.PipeChainStart
defmodule Wocky.TrafficLogSpec do
  use ESpec, async: true

  alias Timex.Duration
  alias Wocky.Repo.Factory
  alias Wocky.Repo.ID
  alias Wocky.TrafficLog

  @packets 100

  before do
    user = Factory.insert(:user)
    resource = ID.new()

    traffic =
      for _ <- 1..@packets do
        Factory.insert(:traffic_log, user: user, resource: resource)
      end

    Factory.insert(:traffic_log, user: user, resource: ID.new())

    {:ok,
     user: user,
     traffic: traffic,
     resource: resource,
     first: hd(traffic).created_at,
     middle: Enum.at(traffic, div(@packets, 2) - 1).created_at,
     last: List.last(traffic).created_at}
  end

  describe "put/1" do
    it "should add a traffic entry for the user" do
      now = Timex.now()
      user = Factory.insert(:user)
      log = Factory.params_for(:traffic_log, user_id: user.id)
      put_result = TrafficLog.put(log)
      put_result |> should(be_ok_result())
      entry = Kernel.elem(put_result, 1)
      entry |> should(be_struct TrafficLog)

      TrafficLog.get_by_period(user.id, now, default_duration())
      |> should(eq [entry])
    end
  end

  describe "get_by_period/3" do
    it "should get all traffic from the specified time/duration" do
      TrafficLog.get_by_period(
        shared.user.id,
        default_start(),
        default_duration()
      )
      |> should(have_length @packets + 1)
    end

    it "should get the subset of packets in the time specified" do
      dur =
        shared.middle
        |> Timex.diff(shared.first)
        |> Duration.from_microseconds()

      TrafficLog.get_by_period(shared.user.id, shared.first, dur)
      |> should(have_length div(@packets, 2))

      TrafficLog.get_by_period(
        shared.user.id,
        shared.last,
        Duration.from_microseconds(0)
      )
      |> should(have_length 1)
    end

    it "should get no traffic for a non-existant user" do
      TrafficLog.get_by_period(ID.new(), default_start(), default_duration())
      |> should(eq [])
    end
  end

  describe "get_by_resource/4" do
    it "should get all traffic from the specified resource/time/duration" do
      TrafficLog.get_by_resource(
        shared.user.id,
        shared.resource,
        default_start(),
        default_duration()
      )
      |> should(have_length @packets)
    end

    it "should get nothing from a different resource" do
      TrafficLog.get_by_resource(
        shared.user.id,
        "fnord",
        default_start(),
        default_duration()
      )
      |> should(eq [])
    end

    it "should get no traffic for a non-existant user" do
      TrafficLog.get_by_resource(
        ID.new(),
        shared.resource,
        default_start(),
        default_duration()
      )
      |> should(eq [])
    end
  end

  defp default_start, do: Timex.subtract(Timex.now(), Duration.from_hours(1))

  defp default_duration, do: Duration.from_hours(1)
end
