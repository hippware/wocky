defmodule WockyAPI.GraphQL.ChannelTest do
  use WockyAPI.SubscriptionCase, async: false

  import WockyAPI.ChannelHelper

  alias Ecto.Adapters.SQL.Sandbox

  setup_all do
    :ok = Sandbox.checkout(Wocky.Repo)
    Sandbox.mode(Wocky.Repo, {:shared, self()})
  end

  describe "hide and check user" do
    test "set user as permanently hidden", %{
      socket: socket,
      token: token,
      user: %{id: user_id}
    } do
      authenticate(user_id, token, socket)

      query = """
      mutation ($enable: Boolean!, $expire: DateTime) {
        userHide (input: {enable: $enable, expire: $expire}) {
          result
        }
      }
      """

      ref = push_doc(socket, query, variables: %{enable: true})
      assert_reply ref, :ok, %{data: %{"userHide" => %{"result" => true}}}, 1000

      query = "query { currentUser { hidden { enabled } } }"

      ref = push_doc(socket, query)
      assert_reply ref, :ok, result, 1000

      assert result.data == %{
               "currentUser" => %{
                 "hidden" => %{
                   "enabled" => true
                 }
               }
             }
    end
  end
end
