defmodule :mod_wocky_notifications_spec do
  use ESpec, sandbox: true
  use SandboxHelper
  use IQHandlerSpec

  import :mod_wocky_notifications

  alias Wocky.PushNotifier.TestNotifier
  alias Wocky.Repo.ID
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
    TestNotifier.reset

    user = Factory.insert(:user)
    {:ok, user: user, user_jid: User.to_jid(user)}
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
        context "on success" do
          before do
            result = enable_notifications(shared.user_jid)
            {:ok, result: result}
          end

          it "should return an IQ result" do
            expect iq(shared.result, :type) |> to(eq :result)
          end

          it "should register the device" do
            [{_, jid, platform, device_id}] = TestNotifier.get_registrations
            expect jid |> to(eq JID.to_binary(shared.user_jid))
            expect platform |> to(eq "apple")
            expect device_id |> to(eq @test_id)
          end
        end

        context "on failure" do
          before do
            result = enable_notifications(shared.user_jid, "error")
            {:ok, result: result}
          end

          it "should return an IQ error" do
            expect iq(shared.result, :type) |> to(eq :error)
          end

          it "should not register the device" do
            expect TestNotifier.get_registrations |> to(eq [])
          end
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

        it "should remove the device registration" do
          expect TestNotifier.get_registrations |> to(eq [])
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
            shared.user_jid, JID.make(ID.new, "localhost"), packet())
        end

        xit "should send a notification" do
          # Wait for the event to be processed
          Process.sleep(50)
          [{_, message}] = TestNotifier.get_notifications
          expect message |> to(end_with "Message content")
        end
      end

      context "with a non-message packet" do
        before do
          :ok = user_send_packet_hook(
            shared.user_jid, @server_jid, packet("parlay"))
        end

        it "should not send a notification" do
          expect TestNotifier.get_notifications |> to(eq [])
        end
      end

      context "with a non-chat message packet" do
        before do
          :ok = user_send_packet_hook(
            shared.user_jid, @server_jid, packet("message", "parlay"))
        end

        it "should not send a notification" do
          expect TestNotifier.get_notifications |> to(eq [])
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

          result = user_send_packet_hook(shared.user_jid, @server_jid, no_body)
          {:ok, result: result}
        end

        it "should not send a notification" do
          expect TestNotifier.get_notifications |> to(eq [])
        end
      end
    end

    describe "handling the remove_user hook" do
      before do
        _ = enable_notifications(shared.user_jid)
        :ok = remove_user_hook(shared.user.username, shared.user.server)
      end

      it "should remove all device registrations" do
        expect TestNotifier.get_registrations |> to(eq [])
      end
    end
  end
end
