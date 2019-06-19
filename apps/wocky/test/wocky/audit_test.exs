defmodule Wocky.AuditTest do
  use Wocky.DataCase, async: false

  import ExUnit.CaptureLog

  alias Wocky.Audit
  alias Wocky.Factory
  alias Wocky.Repo.Factory, as: RepoFactory

  setup do
    orig_level = Logger.level()
    Logger.configure(level: :info)

    on_exit(fn ->
      Logger.configure(level: orig_level)
    end)

    {:ok, user: RepoFactory.build(:user)}
  end

  describe "log_traffic/3" do
    test "should add a traffic entry for the user", %{user: user} do
      log = Factory.build(:traffic_log, user_id: user.id)

      assert capture_log(fn ->
               assert :ok = Audit.log_traffic(log, user, log_traffic: true)
             end) =~ "\"packet\":\"#{log.packet}\""
    end

    test "should not log when :log_traffic is false", %{user: user} do
      log = Factory.build(:traffic_log, user_id: user.id)

      assert capture_log(fn ->
               assert :ok = Audit.log_traffic(log, user, log_traffic: false)
             end) == ""
    end
  end

  describe "log_location/3" do
    test "should add a location entry for the user", %{user: user} do
      loc = RepoFactory.params_for(:location, device: "test", user_id: user.id)

      log =
        capture_log(fn ->
          assert :ok = Audit.log_location(loc, user, log_location: true)
        end)

      assert log =~ "\"lat\":#{loc.lat}"
      assert log =~ "\"lon\":#{loc.lon}"
      assert log =~ "\"user_id\":\"#{user.id}\""
    end

    test "should not log when :log_location is false", %{user: user} do
      loc = RepoFactory.params_for(:location, device: "test")

      assert capture_log(fn ->
               :ok = Audit.log_location(loc, user, log_location: false)
             end) == ""
    end
  end

  describe "log_push/3" do
    test "should add a push notification entry for the user", %{user: user} do
      push = Factory.build(:push_log)

      log =
        capture_log(fn ->
          assert :ok =
                   Audit.log_push(push, user,
                     log_push: true,
                     log_push_payload: true
                   )
        end)

      assert log =~ "\"device\":\"#{push.device}\""
      assert log =~ "\"payload\":#{inspect(push.payload)}"
    end

    test "should not log payload when :log_push_payload is false", %{user: user} do
      push = Factory.build(:push_log)

      log =
        capture_log(fn ->
          assert :ok =
                   Audit.log_push(push, user,
                     log_push: true,
                     log_push_payload: false
                   )
        end)

      refute log =~ "#{inspect(push.payload)}\""
    end

    test "should not log when :log_push is false", %{user: user} do
      push = Factory.build(:push_log)

      assert capture_log(fn ->
               assert :ok = Audit.log_push(push, user, log_push: false)
             end) == ""
    end
  end
end
