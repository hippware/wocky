defmodule NotificationHandlerSpec do
  use ESpec

  alias Wocky.Notification.NullHandler, as: Handler

  @test_jid :jid.make("user", "localhost", "resource")
  @test_id "123456789"

  describe "Enabling a device" do
    before do
      allow Handler |> to(accept :register, fn (_, _, _) -> {:ok, @test_id} end)
      :ok = :wocky_notification_handler.enable(@test_jid, "apple", @test_id)
    end

    it "should call the register function on the handler" do
      expect Handler |> to(accepted :register)
    end
  end

  describe "Sending a message notification" do
    before do
      allow Handler |> to(accept :notify_message, fn (_, _, _) -> :ok end)
      :ok = :wocky_notification_handler.enable(@test_jid, "apple", @test_id)
      :ok = :wocky_notification_handler.notify_message(@test_jid, @test_jid,
                                                       "Message")
    end

    it "should call the notify function on the handler" do
      expect Handler |> to(accepted :notify_message)
    end
  end

  describe "Sending a bot event notification" do
    before do
      allow Handler |> to(accept :notify, fn (_, _) -> :ok end)
      :ok = :wocky_notification_handler.enable(@test_jid, "apple", @test_id)
      :ok = :wocky_notification_handler.notify_bot_event(@test_jid, @test_id,
                                                         :enter)
    end

    it "should call the notify function on the handler" do
      expect Handler |> to(accepted :notify)
    end
  end
end
