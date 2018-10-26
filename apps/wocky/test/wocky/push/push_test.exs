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
    test "should insert the token into the database", ctx do
      row = get_user_token(ctx.user_id, ctx.resource)
      assert row.valid
      assert row.token == ctx.token
      refute is_nil(row.enabled_at)
      assert Timex.diff(row.enabled_at, row.created_at, :seconds) == 0
    end

    test "should overwrite rows when a token is re-enabled", ctx do
      old_row = get_user_token(ctx.user_id, ctx.resource)
      :ok = Push.enable(ctx.user_id, ctx.resource, ctx.token)

      row = get_user_token(ctx.user_id, ctx.resource)
      assert row.valid
      assert row.token == ctx.token
      assert Timex.diff(row.enabled_at, old_row.enabled_at) > 0
    end

    test "should disable old tokens", %{
      user_id: user_id,
      resource: resource,
      token: old_token
    } do
      new_token = Code.isbn13()
      :ok = Push.enable(user_id, resource, new_token)

      old_row =
        Repo.one(
          from Token,
            where: [user_id: ^user_id, resource: ^resource, valid: false]
        )

      assert old_row
      assert old_row.token == old_token

      new_row =
        Repo.one(
          from Token,
            where: [user_id: ^user_id, resource: ^resource, valid: true]
        )

      assert new_row
      assert new_row.token == new_token
    end
  end

  describe "disable/2" do
    setup %{user_id: user_id, resource: resource} do
      :ok = Push.disable(user_id, resource)
    end

    test "should invalidate the database record", ctx do
      row = get_user_token(ctx.user_id, ctx.resource)
      refute is_nil(row)
      refute row.valid
    end
  end

  describe "purge/2" do
    setup %{user_id: user_id} do
      :ok = Push.enable(user_id, "other", "987654321")
      :ok = Push.purge(user_id)
    end

    test "should remove all database records", ctx do
      refute get_user_token(ctx.user_id, ctx.resource)
      refute get_user_token(ctx.user_id, "other")
    end
  end

  describe "notify/3" do
    test "should send a push notification", ctx do
      :ok = Push.notify(ctx.user_id, ctx.resource, @message)

      assert_receive %Notification{
                       payload: %{
                         "aps" => %{"alert" => @message}
                       }
                     },
                     5000
    end

    test "should truncate long messages", ctx do
      sent_message = Lorem.paragraph(100)
      :ok = Push.notify(ctx.user_id, ctx.resource, sent_message)

      assert_receive %Notification{
                       payload: %{
                         "aps" => %{"alert" => rcvd_message}
                       }
                     },
                     5000

      assert String.length(sent_message) > String.length(rcvd_message)
      assert String.slice(rcvd_message, -3..-1) == "..."
    end

    test "should create a db log entry", ctx do
      :ok = Push.notify(ctx.user_id, ctx.resource, @message)
      assert_receive %Notification{}, 5000

      log =
        Repo.one(
          from Log,
            where: [user_id: ^ctx.user_id, resource: ^ctx.resource]
        )

      refute is_nil(log)
    end

    test "should log an error when there is no push token", ctx do
      :ok = Push.disable(ctx.user_id, ctx.resource)

      assert capture_log(fn ->
               :ok = Push.notify(ctx.user_id, ctx.resource, @message)
             end) =~ "no token"
    end

    test "token is invalidated when the service returns an error", ctx do
      assert capture_log(fn ->
               :ok = Push.notify(ctx.user_id, ctx.resource, "bad token")
             end) =~ "device token was bad"

      assert_receive %Notification{response: :bad_device_token}, 5000

      row = get_user_token(ctx.user_id, ctx.resource)
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
         ctx do
      assert capture_log(fn ->
               assert_raise RuntimeError, "requested_raise", fn ->
                 Push.notify(ctx.user_id, ctx.resource, "raise")
               end
             end) == ""

      assert capture_log(fn -> Process.sleep(500) end) =~ "timeout expired"
    end
  end

  describe "retry success" do
    test "should retry and succeed even if the first attempt fails", ctx do
      # Two retries will give us three chunks of log
      assert fn ->
               Push.notify(ctx.user_id, ctx.resource, "retry test")
             end
             |> capture_log()
             |> String.split("PN Error")
             |> length() == 3

      assert_receive %Notification{
                       payload: %{
                         "aps" => %{"alert" => "retry test"}
                       }
                     },
                     5000
    end
  end
end
