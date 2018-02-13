defmodule :mod_wocky_notifications_spec do
  use ESpec, sandbox: true
  use SandboxHelper
  use IQHandlerSpec

  import :mod_wocky_notifications

  alias Pigeon.APNS.Notification
  alias Wocky.Push.Sandbox
  alias Wocky.Repo.Factory
  alias Wocky.User

  @test_id "123456789"
  @notify_timeout 10000

  def enable_notifications(user_jid, device \\ @test_id) do
    iq_set =
      iq(
        type: :set,
        sub_el:
          xmlel(
            name: "enable",
            attrs: [{"device", device}, {"platform", "apple"}]
          )
      )

    handle_iq(user_jid, @server_jid, iq_set)
  end

  def disable_notifications(user_jid) do
    iq_set =
      iq(
        type: :set,
        sub_el: xmlel(name: "disable")
      )

    handle_iq(user_jid, @server_jid, iq_set)
  end

  def packet(name \\ "message", type \\ "chat") do
    xmlel(
      name: name,
      attrs: [{"type", type}],
      children: [
        xmlel(
          name: "body",
          children: [xmlcdata(content: "Message content")]
        )
      ]
    )
  end

  def image_packet do
    xmlel(
      name: "message",
      attrs: [{"type", "chat"}],
      children: [
        xmlel(
          name: "image",
          children: [
            xmlel(
              name: "url",
              children: [xmlcdata(content: "Image URL")]
            )
          ]
        )
      ]
    )
  end

  def combo_packet do
    xmlel(
      name: "message",
      attrs: [{"type", "chat"}],
      children: [
        xmlel(
          name: "body",
          children: [xmlcdata(content: "Message content")]
        ),
        xmlel(
          name: "image",
          children: [
            xmlel(
              name: "url",
              children: [xmlcdata(content: "Image URL")]
            )
          ]
        )
      ]
    )
  end

  before do
    Sandbox.start_link()
    sender = Factory.insert(:user, resource: "testing")
    user = Factory.insert(:user, resource: "testing")

    {:ok,
     user: user,
     user_jid: User.to_jid(user),
     sender: sender,
     sender_jid: User.to_jid(sender)}
  end

  describe "mod_wocky_notifications" do
    describe "handling an IQ 'get'" do
      it "should return an error result" do
        result = handle_iq(shared.user_jid, @server_jid, iq(type: :get))
        iq(result, :type) |> should(eq :error)
      end
    end

    describe "handling an IQ 'set'" do
      context "with an 'enable' element" do
        before do
          result = enable_notifications(shared.user_jid)
          {:ok, result: result}
        end

        it "should return an IQ result" do
          iq(shared.result, :type) |> should(eq :result)
        end
      end

      context "with a 'disable' element" do
        before do
          _ = enable_notifications(shared.user_jid)
          result = disable_notifications(shared.user_jid)
          {:ok, result: result}
        end

        it "should return an IQ result" do
          iq(shared.result, :type) |> should(eq :result)
        end
      end
    end

    describe "handling the user_send_packet hook" do
      before do
        Sandbox.clear_notifications()

        _ = enable_notifications(shared.user_jid)
        :ok
      end

      context "with a message packet" do
        before do
          %{result: :ok} =
            user_send_packet_hook(
              %{},
              shared.sender_jid,
              shared.user_jid,
              packet()
            )
        end

        it "should send a notification" do
          notifications =
            Sandbox.wait_notifications(count: 1, timeout: @notify_timeout)

          notifications |> should(have_size 1)

          [%Notification{payload: payload}] = notifications
          payload["aps"]["alert"] |> should(end_with "Message content")
        end
      end

      context "with an image message packet" do
        before do
          %{result: :ok} =
            user_send_packet_hook(
              %{},
              shared.sender_jid,
              shared.user_jid,
              image_packet()
            )
        end

        it "should send a notification" do
          notifications =
            Sandbox.wait_notifications(count: 1, timeout: @notify_timeout)

          notifications |> should(have_size 1)

          [%Notification{payload: payload}] = notifications
          payload["aps"]["alert"] |> should(end_with "sent you an image!")
        end
      end

      context "with a message that contains both a body and an image" do
        before do
          %{result: :ok} =
            user_send_packet_hook(
              %{},
              shared.sender_jid,
              shared.user_jid,
              combo_packet()
            )
        end

        it "should send a notification" do
          notifications =
            Sandbox.wait_notifications(count: 1, timeout: @notify_timeout)

          notifications |> should(have_size 1)

          [%Notification{payload: payload}] = notifications
          payload["aps"]["alert"] |> should(end_with "Message content")
        end
      end

      context "with a non-message packet" do
        before do
          %{result: :ok} =
            user_send_packet_hook(
              %{},
              shared.sender_jid,
              shared.user_jid,
              packet("parlay")
            )
        end

        it "should not send a notification" do
          Sandbox.list_notifications() |> should(eq [])
        end
      end

      context "with a non-chat message packet" do
        before do
          %{result: :ok} =
            user_send_packet_hook(
              %{},
              shared.sender_jid,
              shared.user_jid,
              packet("message", "parlay")
            )
        end

        it "should not send a notification" do
          Sandbox.list_notifications() |> should(eq [])
        end
      end

      context "with a packet with no body and no image" do
        before do
          no_body =
            xmlel(
              name: "message",
              attrs: [{"type", "chat"}],
              children: [
                xmlel(
                  name: "content",
                  children: [xmlcdata(content: "Message content")]
                )
              ]
            )

          result =
            user_send_packet_hook(
              %{},
              shared.sender_jid,
              shared.user_jid,
              no_body
            )

          {:ok, result: result}
        end

        it "should not send a notification" do
          Sandbox.list_notifications() |> should(eq [])
        end
      end
    end

    describe "handling the remove_user hook" do
      before do
        _ = enable_notifications(shared.user_jid)
        result = remove_user_hook(:ok, shared.user.username, shared.user.server)
        {:ok, result: result}
      end

      it "should return :ok" do
        shared.result |> should(eq :ok)
      end
    end
  end
end
