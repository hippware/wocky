defmodule Wocky.ModWockyNotificationsSpec do
  use ESpec

  alias Wocky.Notification.NullHandler, as: Handler

  require Record
  import Record, only: [defrecord: 2, extract: 2]

  defrecord :xmlel, extract(:xmlel, from_lib: "exml/include/exml.hrl")
  defrecord :xmlcdata, extract(:xmlcdata, from_lib: "exml/include/exml.hrl")
  defrecord :iq, extract(:iq, from_lib: "ejabberd/include/jlib.hrl")
  # defrecord :jid, extract(:jid, from_lib: "ejabberd/include/jlib.hrl")

  @user          "043e8c96-ba30-11e5-9912-ba0be0483c18"
  @server        "localhost"
  @resource      "testing"
  @local_context "localhost"
  @test_jid      {@user, @server, @resource}
  @test_id       "123456789"

  def jid do
    :jid.make(@user, @server, @resource)
  end

  def enable_notifications do
    iq_set = iq(
      type: :set,
      sub_el: xmlel(
        name: "enable",
        attrs: [{"device", @test_id}]
      )
    )
    :mod_wocky_notifications.handle_iq(jid(), jid(), iq_set)
  end

  def disable_notifications do
    iq_set = iq(
      type: :set,
      sub_el: xmlel(
        name: "disable"
      )
    )
    :mod_wocky_notifications.handle_iq(jid(), jid(), iq_set)
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

  describe "mod_wocky_notifications" do
    before do
      :ok = :wocky_db.prepare_tables(@local_context, [:device])
      allow Handler |> to(accept :register, fn (_, _) -> {:ok, @test_id} end)
      allow Handler |> to(accept :notify, fn (_, _, _) -> :ok end)
      IO.puts "running top before block"
    end

    describe "Handling an IQ 'get'" do
      it "should return an error result" do
        result = :mod_wocky_notifications.handle_iq(jid(), jid(), iq(type: :get))
        expect iq(result, :type) |> to(eq :error)
      end
    end

    describe "Handling an IQ 'set'" do
      describe "with an 'enable' element" do
        before do
          result = enable_notifications
          {:shared, result: result}
        end

        it "should return an IQ result" do
          expect iq(shared.result, :type) |> to(eq :result)
        end

        it "should register the device" do
          expect Handler |> to(accepted :register)
        end

        it "should insert the device_id and endpoint into the database" do
          row = :wocky_db.select_row(@local_context, :device, :all,
            %{user: @user, server: @server, resource: @resource})

          expect row.device_id |> to(eq @test_id)
          expect row.endpoint |> to(eq @test_id)
        end
      end

      describe "with a 'disable' element" do
        before do
          _ = enable_notifications
          result = disable_notifications
          {:shared, result: result}
        end

        it "should return an IQ result" do
          expect iq(shared.result, :type) |> to(eq :result)
        end

        it "should remove the device_id and endpoint from the database" do
          row = :wocky_db.select_row(@local_context, :device, :all,
            %{user: @user, server: @server, resource: @resource})

          expect row |> to(eq :not_found)
        end
      end
    end

    describe "handling the offline_message hook" do
      before do
        _ = enable_notifications
        :ok = :mod_wocky_notifications.offline_message_hook(jid(), jid(), packet)
      end

      it "should send a notification" do
        expect Handler |> to(accepted :notify)
      end
    end

    describe "handling the user_received_packet hook" do
      before do
        _ = enable_notifications
      end

      describe "with a message packet" do
        before do
          :ok = :mod_wocky_notifications.user_receive_packet_hook(
            jid(), jid(), jid(), packet)
        end

        it "should send a notification" do
          expect Handler |> to(accepted :notify)
        end
      end

      describe "with a non-message packet" do
        before do
          :ok = :mod_wocky_notifications.user_receive_packet_hook(
            jid(), jid(), jid(), packet("parlay"))
        end

        it "should not send a notification" do
          expect Handler |> to_not(accepted :notify)
        end
      end

      describe "with a non-chat message packet" do
        before do
          :ok = :mod_wocky_notifications.user_receive_packet_hook(
            jid(), jid(), jid(), packet("message", "parlay"))
        end

        it "should not send a notification" do
          expect Handler |> to_not(accepted :notify)
        end
      end

      describe "with a packet with no body" do
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

          :ok = :mod_wocky_notifications.user_receive_packet_hook(
            jid(), jid(), jid(), no_body)
        end

        it "should not send a notification" do
          expect Handler |> to_not(accepted :notify)
        end
      end
    end
  end
end
