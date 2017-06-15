defmodule :mod_wocky_notifications_spec do
  use ESpec, sandbox: true
  use SandboxHelper
  use IQHandlerSpec

  import :mod_wocky_notifications

  alias Pushex.Sandbox
  alias Wocky.Repo.Factory
  alias Wocky.User

  @test_id       "123456789"

  def enable_notifications(user_jid, device \\ @test_id) do
    iq_set = iq(
      type: :set,
      sub_el: xmlel(
        name: "enable",
        attrs: [{"device", device}, {"platform", "apple"}]
      )
    )
    handle_iq(user_jid, @server_jid, iq_set)
  end

  def disable_notifications(user_jid) do
    iq_set = iq(
      type: :set,
      sub_el: xmlel(
        name: "disable"
      )
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

  before do
    pid = GenServer.whereis(:push_notification_event_handler)
    Ecto.Adapters.SQL.Sandbox.allow(Wocky.Repo, self(), pid)

    Sandbox.clear_notifications(pid: pid)

    sender = Factory.insert(:user)
    user = Factory.insert(:user)
    {:ok, pid: pid, user: user, user_jid: User.to_jid(user),
          sender: sender, sender_jid: User.to_jid(sender)}
  end

  describe "mod_wocky_notifications" do
    describe "handling an IQ 'get'" do
      it "should return an error result" do
        result = handle_iq(shared.user_jid, @server_jid, iq(type: :get))
        expect iq(result, :type) |> to(eq :error)
      end
    end

    describe "handling an IQ 'set'" do
      context "with an 'enable' element" do
        before do
          result = enable_notifications(shared.user_jid)
          {:ok, result: result}
        end

        it "should return an IQ result" do
          expect iq(shared.result, :type) |> to(eq :result)
        end
      end

      context "with a 'disable' element" do
        before do
          _ = enable_notifications(shared.user_jid)
          result = disable_notifications(shared.user_jid)
          {:ok, result: result}
        end

        it "should return an IQ result" do
          expect iq(shared.result, :type) |> to(eq :result)
        end
      end
    end

    describe "handling the user_send_packet hook" do
      before do
        _ = enable_notifications(shared.user_jid)
      end

      context "with a message packet" do
        before do
          :ok = user_send_packet_hook(
            shared.sender_jid, shared.user_jid, packet())
        end

        it "should send a notification" do
          notifications = Sandbox.wait_notifications(count: 1, pid: shared.pid)
          notifications |> should(have_size 1)

          [{{:ok, _}, request, _}] = notifications
          request.notification.alert |> should(end_with "Message content")
        end
      end

      context "with a non-message packet" do
        before do
          :ok = user_send_packet_hook(
            shared.sender_jid, shared.user_jid, packet("parlay"))
        end

        it "should not send a notification" do
          expect Sandbox.list_notifications(pid: shared.pid) |> to(eq [])
        end
      end

      context "with a non-chat message packet" do
        before do
          :ok = user_send_packet_hook(
            shared.sender_jid, shared.user_jid, packet("message", "parlay"))
        end

        it "should not send a notification" do
          expect Sandbox.list_notifications(pid: shared.pid) |> to(eq [])
        end
      end

      context "with a packet with no body" do
        before do
          no_body = xmlel(
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
            user_send_packet_hook(shared.sender_jid, shared.user_jid, no_body)
          {:ok, result: result}
        end

        it "should not send a notification" do
          expect Sandbox.list_notifications(pid: shared.pid) |> to(eq [])
        end
      end
    end

    describe "handling the remove_user hook" do
      before do
        _ = enable_notifications(shared.user_jid)
        result = remove_user_hook(shared.user.username, shared.user.server)
        {:ok, result: result}
      end

      it "should return :ok" do
        shared.result |> should(eq :ok)
      end
    end
  end
end
