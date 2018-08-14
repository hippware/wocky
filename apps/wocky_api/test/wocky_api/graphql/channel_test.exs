defmodule WockyAPI.GraphQL.ChannelTest do
  use WockyAPI.SubscriptionCase, async: false

  import WockyAPI.ChannelHelper
  import WockyAPI.GraphQLHelper

  alias Ecto.Adapters.SQL.Sandbox
  alias Wocky.Repo.Factory

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

  describe "bot access" do
    test "preallocate, update and get bot", %{
      socket: socket,
      token: token,
      user: %{id: user_id}
    } do
      authenticate(user_id, token, socket)
      query = "mutation { botCreate { successful, result { id } } }"

      ref = push_doc(socket, query)
      assert_reply ref, :ok, result, 1000

      assert %{
               "botCreate" => %{
                 "successful" => true,
                 "result" => %{
                   "id" => id
                 }
               }
             } = result.data

      query = """
      mutation ($id: UUID!, $values: BotParams!) {
        botUpdate (input: {id: $id, values: $values}) {
          successful
          result {
            id
          }
        }
      }
      """

      values =
        :bot
        |> Factory.build()
        |> add_bot_lat_lon()
        |> Map.take(bot_create_fields())

      ref =
        push_doc(socket, query,
          variables: %{
            "id" => id,
            "values" => values
          }
        )

      assert_reply ref, :ok, _result, 1000

      query = "query ($id: UUID!) { bot (id: $id) { title } }"

      ref = push_doc(socket, query, variables: %{"id" => id})
      assert_reply ref, :ok, result, 1000

      title = values[:title]

      assert %{
               "bot" => %{
                 "title" => ^title
               }
             } = result.data
    end
  end
end
