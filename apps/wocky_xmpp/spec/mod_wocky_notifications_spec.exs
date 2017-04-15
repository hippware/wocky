defmodule ModWockyNotificationsSpec do
  use ESpec
  use Wocky.JID

  import Record, only: [defrecordp: 2, extract: 2]

  alias Wocky.PushNotifier.TestBackend
  alias :wocky_db, as: WockyDb
  alias :mod_wocky_notifications, as: ModWockyNotifications

  require Record

  defrecordp :xmlel, extract(:xmlel, from_lib: "exml/include/exml.hrl")
  defrecordp :xmlcdata, extract(:xmlcdata, from_lib: "exml/include/exml.hrl")
  defrecordp :iq, extract(:iq, from_lib: "ejabberd/include/jlib.hrl")

  @user          "043e8c96-ba30-11e5-9912-ba0be0483c18"
  @server        "localhost"
  @resource      "testing"
  @local_context "localhost"
  @jid           JID.make(@user, @server, @resource)
  @test_id       "123456789"

  def enable_notifications(device \\ @test_id) do
    iq_set = iq(
      type: :set,
      sub_el: xmlel(
        name: "enable",
        attrs: [{"device", device}, {"platform", "apple"}]
      )
    )
    ModWockyNotifications.handle_iq(@jid, @jid, iq_set)
  end

  def disable_notifications do
    iq_set = iq(
      type: :set,
      sub_el: xmlel(
        name: "disable"
      )
    )
    ModWockyNotifications.handle_iq(@jid, @jid, iq_set)
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
    WockyDb.clear_tables(@local_context, [:device])
    TestBackend.reset
  end

  describe "mod_wocky_notifications" do
    describe "handling an IQ 'get'" do
      it "should return an error result" do
        result = ModWockyNotifications.handle_iq(@jid, @jid, iq(type: :get))
        expect iq(result, :type) |> to(eq :error)
      end
    end

    describe "handling an IQ 'set'" do
      context "with an 'enable' element" do
        context "on success" do
          before do
            result = enable_notifications()
            {:ok, result: result}
          end

          it "should return an IQ result" do
            expect iq(shared.result, :type) |> to(eq :result)
          end

          it "should register the device" do
            [{_, jid, platform, device_id}] = TestBackend.get_registrations
            expect jid |> to(eq JID.to_binary(@jid))
            expect platform |> to(eq "apple")
            expect device_id |> to(eq @test_id)
          end

          it "should insert the device_id and endpoint into the database" do
            row = WockyDb.select_row(@local_context, :device, :all,
              %{user: @user, server: @server, resource: @resource})

            expect row.device_id |> to(eq @test_id)
          end
        end

        context "on failure" do
          before do
            result = enable_notifications("error")
            {:ok, result: result}
          end

          it "should return an IQ error" do
            expect iq(shared.result, :type) |> to(eq :error)
          end

          it "should not register the device" do
            expect TestBackend.get_registrations |> to(eq [])
          end

          it "should not insert anything into the database" do
            row = WockyDb.select_row(@local_context, :device, :all,
              %{user: @user, server: @server, resource: @resource})

            expect row |> to(eq :not_found)
          end
        end
      end

      context "with a 'disable' element" do
        before do
          _ = enable_notifications()
          result = disable_notifications()
          {:ok, result: result}
        end

        it "should return an IQ result" do
          expect iq(shared.result, :type) |> to(eq :result)
        end

        it "should remove the device registration" do
          expect TestBackend.get_registrations |> to(eq [])
        end

        it "should remove the device_id and endpoint from the database" do
          row = WockyDb.select_row(@local_context, :device, :all,
            %{user: @user, server: @server, resource: @resource})

          expect row |> to(eq :not_found)
        end
      end
    end

    describe "handling the user_send_packet hook" do
      before do
        _ = enable_notifications()
      end

      context "with a message packet" do
        before do
          :ok = ModWockyNotifications.user_send_packet_hook(
            @jid, @jid, packet())
        end

        it "should send a notification" do
          # Wait for the event to be processed
          Process.sleep(50)
          [{_, message}] = TestBackend.get_notifications
          expect message |> to(end_with "Message content")
        end
      end

      context "with a non-message packet" do
        before do
          :ok = ModWockyNotifications.user_send_packet_hook(
            @jid, @jid, packet("parlay"))
        end

        it "should not send a notification" do
          expect TestBackend.get_notifications |> to(eq [])
        end
      end

      context "with a non-chat message packet" do
        before do
          :ok = ModWockyNotifications.user_send_packet_hook(
            @jid, @jid, packet("message", "parlay"))
        end

        it "should not send a notification" do
          expect TestBackend.get_notifications |> to(eq [])
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

          result = ModWockyNotifications.user_send_packet_hook(
            @jid, @jid, no_body)
          {:ok, result: result}
        end

        it "should not send a notification" do
          expect TestBackend.get_notifications |> to(eq [])
        end
      end
    end

    describe "handling the remove_user hook" do
      before do
        _ = enable_notifications()
        :ok = ModWockyNotifications.remove_user_hook(@user, @server)
        :ok
      end

      it "should remove all device registrations" do
        expect TestBackend.get_registrations |> to(eq [])
      end

      it "should remove all user records" do
        row = WockyDb.select_row(@local_context, :device, :all,
          %{user: @user, server: @server, resource: @resource})

        expect row |> to(eq :not_found)
      end
    end
  end
end
