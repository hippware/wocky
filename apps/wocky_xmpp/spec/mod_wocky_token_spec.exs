defmodule :mod_wocky_token_spec do
  use ESpec, async: true, sandbox: true
  use SandboxHelper
  use IQHandlerSpec

  import :mod_wocky_token, only: [handle_iq: 3]

  alias Wocky.Repo.Factory
  alias Wocky.User

  @iso8601_regex ~r/\d\d\d\d-\d\d-\d\dT\d\d:\d\d:\d\d\+\d\d:\d\d/

  def result_iq(content) do
    iq(
      type: :result,
      sub_el: [
        xmlel(
          name: "query",
          attrs: [{"xmlns", "hippware.com/hxep/token"}],
          children: content
        )
      ]
    )
  end

  before do
    user = Factory.insert(:user, resource: "testing")
    {:ok, user: user, user_jid: User.to_jid(user)}
  end

  describe "mod_wocky_token" do
    describe "handling an IQ 'get'" do
      context "when the JID domainpart is not local" do
        before do
          result = handle_iq(jid(lserver: "remote"), @server_jid, iq())
          {:ok, result: result}
        end

        it "should return an error" do
          iq(type: type) = shared.result
          type |> should(eq :error)
        end
      end

      context "when the JID domainpart is local" do
        before do
          result = handle_iq(shared.user_jid, @server_jid, iq(type: :get))
          {:ok, result: result}
        end

        it "should return a result IQ" do
          iq(type: type) = shared.result
          type |> should(eq :result)
        end

        it "should return a token" do
          iq(
            sub_el: [
              xmlel(children: [xmlcdata(content: content)])
            ]
          ) = shared.result

          content |> should(start_with "$T$")
        end

        it "should return a token expiry" do
          iq(sub_el: [xmlel(attrs: [_, {"expiry", expiry}])]) = shared.result
          expiry |> should(match @iso8601_regex)
        end
      end
    end

    describe "handling an IQ 'set'" do
      before do
        result = handle_iq(shared.user_jid, @server_jid, iq(type: :set))
        {:ok, result: result}
      end

      it "should return an empty result IQ" do
        shared.result |> should(eq result_iq([]))
      end
    end
  end
end
