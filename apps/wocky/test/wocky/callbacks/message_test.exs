defmodule Wocky.Callbacks.MessageTest do
  use Wocky.WatcherCase

  alias Faker.Code
  alias Faker.Lorem
  alias Pigeon.APNS.Notification
  alias Wocky.Callbacks.Message, as: Callback
  alias Wocky.Notifier.Push
  alias Wocky.Notifier.Push.Backend.Sandbox
  alias Wocky.Repo.Factory

  setup_all do
    Callback.register()
  end

  describe "push notifications" do
    setup do
      user = Factory.insert(:user, device: "testing")
      Sandbox.clear_notifications(global: true)
      Push.enable(user, user.device, Code.isbn13())

      {:ok, user: user}
    end

    test "on message receipt", %{user: user} do
      text = Lorem.paragraph()
      m = Factory.insert(:message, content: text, recipient: user)

      msgs = Sandbox.wait_notifications(count: 1, timeout: 500, global: true)
      assert length(msgs) == 1

      assert %Notification{
               payload: %{
                 "aps" => %{"alert" => message}
               }
             } = hd(msgs)

      assert message == "From: @#{m.sender.handle}\n#{text}"
    end

    test "on image receipt", %{user: user} do
      text = Lorem.sentence()

      m =
        Factory.insert(:message, content: nil, image_url: text, recipient: user)

      msgs = Sandbox.wait_notifications(count: 1, timeout: 500, global: true)
      assert length(msgs) == 1

      assert %Notification{
               payload: %{
                 "aps" => %{"alert" => message}
               }
             } = hd(msgs)

      assert message == "@#{m.sender.handle} sent you an image."
    end
  end
end
