defmodule Wocky.NotificationHandlerSpec do
  use ESpec

  alias Wocky.Notification.NullHandler, as: Handler

  @test_jid {"user", "server", "resource"}
  @test_id "123456789"

  describe "Registering a device" do
    before do
      allow Handler |> to(accept :register, fn (_, _, _) -> {:ok, @test_id} end)
      {:ok, _return} =
          :wocky_notification_handler.register(@test_jid, "apple", @test_id)
    end

    it "should call the register function on the handler" do
      expect Handler |> to(accepted :register)
    end
  end

  describe "Notifying a client" do
    before do
      allow Handler |> to(accept :notify, fn (_, _, _) -> :ok end)
      :ok = :wocky_notification_handler.notify(@test_id, @test_jid, "Message")
    end

    it "should call the notify function on the handler" do
      expect Handler |> to(accepted :notify)
    end
  end
end
