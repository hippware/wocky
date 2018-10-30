# credo:disable-for-this-file Credo.Check.Refactor.PipeChainStart
defmodule :mod_wocky_notifications_spec do
  use ESpec, sandbox: true
  use SandboxHelper
  use IQHandlerSpec

  import :mod_wocky_notifications

  alias Wocky.Push.Sandbox
  alias Wocky.Repo.Factory
  alias Wocky.User

  @test_id "123456789"

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

    describe "handling the remove_user hook" do
      before do
        _ = enable_notifications(shared.user_jid)
        result = remove_user_hook(:ok, shared.user.username, Wocky.host())
        {:ok, result: result}
      end

      it "should return :ok" do
        shared.result |> should(eq :ok)
      end
    end
  end
end
