defmodule Wocky.PushNotifierSpec do
  use ESpec
  use Wocky.JID

  alias Wocky.PushNotifier
  alias Wocky.PushNotifier.NullBackend, as: Backend

  @test_jid JID.make("user", "localhost", "resource")
  @test_id "123456789"

  before do
    allow :ejabberd_sm |> to(accept :get_user_resources,
                             fn (_, _) -> ["resource"] end)
  end

  describe "Enabling a device" do
    before do
      allow Backend
      |> to(accept :enable, fn (_, _, _, _, _) -> {:ok, @test_id} end)

      :ok = PushNotifier.enable(@test_jid, "apple", @test_id)
    end

    it "should call the enable function on the handler" do
      expect Backend |> to(accepted :enable)
    end
  end

  describe "Sending a notification" do
    before do
      allow Backend |> to(accept :push, fn (_, _) -> :ok end)
      :ok = PushNotifier.enable(@test_jid, "apple", @test_id)
      :ok = PushNotifier.push(@test_jid, "Message")
    end

    it "should call the notify function on the handler" do
      expect Backend |> to(accepted :push)
    end
  end
end
