defmodule Wocky.Notifier.Push.PushTest do
  use Wocky.DataCase, async: true

  import ExUnit.CaptureLog

  alias Faker.Code
  alias Faker.Lorem
  alias Pigeon.APNS.Notification
  alias Wocky.Notifier.Push
  alias Wocky.Notifier.Push.Backend.Sandbox
  alias Wocky.Notifier.Push.Token
  alias Wocky.Repo
  alias Wocky.Repo.Factory

  require Logger

  @message "Message content"

  setup do
    Sandbox.clear_notifications()

    original_timeout = Push.get_config(:timeout)
    set_timeout(200)

    user = Factory.insert(:user, device: "testing")
    token = Code.isbn13()

    :ok = Push.enable(user, user.device, token)

    on_exit(fn ->
      set_timeout(original_timeout)
    end)

    {:ok, user: user, device: user.device, token: token}
  end

  defp set_timeout(timeout) do
    Push.put_config(:timeout, timeout)
  end

  defp get_user_token(user, device) do
    Repo.one(from Token, where: [user_id: ^user.id, device: ^device])
  end

  describe "enable/4" do
    test "should insert the token into the database", ctx do
      row = get_user_token(ctx.user, ctx.device)
      assert row.valid
      assert row.token == ctx.token
      refute is_nil(row.enabled_at)
      assert Timex.diff(row.enabled_at, row.created_at, :seconds) == 0
    end

    test "should overwrite rows when a token is re-enabled", ctx do
      old_row = get_user_token(ctx.user, ctx.device)
      :ok = Push.enable(ctx.user, ctx.device, ctx.token)

      row = get_user_token(ctx.user, ctx.device)
      assert row.valid
      assert row.token == ctx.token
      assert Timex.diff(row.enabled_at, old_row.enabled_at) > 0
    end

    test "should update dev mode on existing tokens", ctx do
      old_row = get_user_token(ctx.user, ctx.device)
      :ok = Push.enable(ctx.user, ctx.device, ctx.token, :apns, true)

      row = get_user_token(ctx.user, ctx.device)
      assert row.valid
      assert row.dev_mode
      assert Timex.diff(row.enabled_at, old_row.enabled_at) > 0
    end

    test "should disable old tokens", %{
      user: user,
      device: device,
      token: old_token
    } do
      new_token = Code.isbn13()
      :ok = Push.enable(user, device, new_token)

      old_row =
        Repo.one(
          from Token,
            where: [user_id: ^user.id, device: ^device, valid: false]
        )

      assert old_row
      assert old_row.token == old_token

      new_row =
        Repo.one(
          from Token,
            where: [user_id: ^user.id, device: ^device, valid: true]
        )

      assert new_row
      assert new_row.token == new_token
    end
  end

  describe "disable/2" do
    setup %{user: user, device: device} do
      :ok = Push.disable(user, device)
    end

    test "should invalidate the database record", ctx do
      row = get_user_token(ctx.user, ctx.device)
      refute is_nil(row)
      refute row.valid
    end
  end

  describe "notify_all/2" do
    test "should send a push notification", ctx do
      :ok = Push.notify_all(ctx.user, @message)

      assert_receive %Notification{
                       payload: %{
                         "aps" => %{"alert" => @message}
                       }
                     },
                     5000
    end

    test "should send a push notification to each endpoint", ctx do
      :ok = Push.enable(ctx.user, "other", "987654321")
      :ok = Push.notify_all(ctx.user, @message)

      notifications = Sandbox.wait_notifications(count: 2, timeout: 5000)
      assert Enum.count(notifications) == 2
    end

    test "should truncate long messages", ctx do
      sent_message = Lorem.paragraph(100)
      :ok = Push.notify_all(ctx.user, sent_message)

      assert_receive %Notification{
                       payload: %{
                         "aps" => %{"alert" => rcvd_message}
                       }
                     },
                     5000

      assert String.length(sent_message) > String.length(rcvd_message)
      assert String.slice(rcvd_message, -3..-1) == "..."
    end

    test "token is invalidated when the service returns an error", ctx do
      assert capture_log(fn ->
               :ok = Push.notify_all(ctx.user, "bad token")
             end) =~ "device token was bad"

      assert_receive %Notification{response: :bad_device_token}, 5000

      row = get_user_token(ctx.user, ctx.device)
      refute row.valid
      refute is_nil(row.invalidated_at)
    end
  end

  # TODO This is an important case to test, but I am not sure that this is the
  # right way to test it. This test ends up with spurious logging output to the
  # console when the test suite runs.
  # describe "push timeout" do
  #   test "should log an error when the push request fails and times out",
  #        ctx do
  #     assert capture_log(fn ->
  #              assert_raise RuntimeError, "requested_raise", fn ->
  #                Push.notify_all(ctx.user, "raise")
  #              end
  #            end) == ""

  #     assert capture_log(fn -> Process.sleep(500) end) =~ "timeout expired"
  #   end
  # end

  describe "retry success" do
    test "should retry and succeed even if the first attempt fails", ctx do
      # Two retries will give us three chunks of log
      assert fn ->
               Push.notify_all(ctx.user, "retry test")
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
