defmodule Wocky.PushTest do
  use Wocky.DataCase

  import ExUnit.CaptureLog

  alias Faker.Code
  alias Faker.Lorem
  alias Pigeon.APNS.Notification
  alias Wocky.Push
  alias Wocky.Push.Log
  alias Wocky.Push.Sandbox
  alias Wocky.Push.Token
  alias Wocky.Repo
  alias Wocky.Repo.Factory

  require Logger

  @message "Message content"

  setup do
    Sandbox.clear_notifications()

    original_timeout = Confex.get_env(:wocky, Wocky.Push)[:timeout]
    set_timeout(200)

    user = Factory.insert(:user, resource: "testing")
    token = Code.isbn13()

    :ok = Push.enable(user.id, user.resource, token)

    on_exit(fn ->
      set_timeout(original_timeout)
    end)

    {:ok, user_id: user.id, resource: user.resource, token: token}
  end

  defp set_timeout(timeout) do
    config =
      :wocky
      |> Application.get_env(Wocky.Push)
      |> Keyword.replace!(:timeout, timeout)

    Application.put_env(:wocky, Wocky.Push, config)
  end

  defp get_user_token(user_id, resource) do
    Repo.one(from Token, where: [user_id: ^user_id, resource: ^resource])
  end

  describe "enable/4" do
    test "should insert the token into the database", shared do
      row = get_user_token(shared.user_id, shared.resource)
      assert row.valid
      assert row.token == shared.token
      refute is_nil(row.enabled_at)
      assert Timex.diff(row.enabled_at, row.created_at, :seconds) == 0
    end

    test "should overwrite rows when a token is re-enabled", shared do
      old_row = get_user_token(shared.user_id, shared.resource)
      :ok = Push.enable(shared.user_id, shared.resource, shared.token)

      row = get_user_token(shared.user_id, shared.resource)
      assert row.valid
      assert row.token == shared.token
      assert Timex.diff(row.enabled_at, old_row.enabled_at) > 0
    end
  end

  describe "disable/2" do
    setup %{user_id: user_id, resource: resource} do
      :ok = Push.disable(user_id, resource)
    end

    test "should invalidate the database record", shared do
      row = get_user_token(shared.user_id, shared.resource)
      refute is_nil(row)
      refute row.valid
    end
  end

  describe "purge/2" do
    setup %{user_id: user_id} do
      :ok = Push.enable(user_id, "other", "987654321")
      :ok = Push.purge(user_id)
    end

    test "should remove all database records", shared do
      refute get_user_token(shared.user_id, shared.resource)
      refute get_user_token(shared.user_id, "other")
    end
  end

  describe "notify/3" do
    test "should send a push notification", shared do
      :ok = Push.notify(shared.user_id, shared.resource, @message)

      assert_receive %Notification{
                       payload: %{
                         "aps" => %{"alert" => @message}
                       }
                     },
                     5000
    end

    test "should truncate long messages", shared do
      sent_message = Lorem.paragraph(100)
      :ok = Push.notify(shared.user_id, shared.resource, sent_message)

      assert_receive %Notification{
                       payload: %{
                         "aps" => %{"alert" => rcvd_message}
                       }
                     },
                     5000

      assert String.length(sent_message) > String.length(rcvd_message)
      assert String.slice(rcvd_message, -3..-1) == "..."
    end

    test "should create a db log entry", shared do
      :ok = Push.notify(shared.user_id, shared.resource, @message)
      assert_receive %Notification{}, 5000

      log =
        Repo.one(
          from Log,
            where: [user_id: ^shared.user_id, resource: ^shared.resource]
        )

      refute is_nil(log)
    end

    test "should log an error when there is no push token", shared do
      :ok = Push.disable(shared.user_id, shared.resource)

      assert capture_log(fn ->
               :ok = Push.notify(shared.user_id, shared.resource, @message)
             end) =~ "no token"
    end

    test "token is invalidated when the service returns an error", shared do
      assert capture_log(fn ->
               :ok = Push.notify(shared.user_id, shared.resource, "bad token")
             end) =~ "device token was bad"

      assert_receive %Notification{response: :bad_device_token}, 5000

      row = get_user_token(shared.user_id, shared.resource)
      refute row.valid
      refute is_nil(row.invalidated_at)
    end
  end

  describe "notify_all/2" do
    setup %{user_id: user_id} do
      :ok = Push.enable(user_id, "other", "987654321")
      :ok = Push.notify_all(user_id, @message)
    end

    test "should send a push notification to each endpoint" do
      notifications = Sandbox.wait_notifications(count: 2, timeout: 5000)
      assert Enum.count(notifications) == 2
    end
  end

  describe "push timeout" do
    test "should log an error when the push request fails and times out",
         shared do
      assert capture_log(fn ->
               assert_raise RuntimeError, "requested_raise", fn ->
                 Push.notify(shared.user_id, shared.resource, "raise")
               end
             end) == ""

      assert capture_log(fn -> Process.sleep(500) end) =~ "timeout expired"
    end
  end
end
