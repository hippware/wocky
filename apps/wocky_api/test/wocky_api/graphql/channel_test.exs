defmodule WockyAPI.GraphQL.ChannelTest do
  use WockyAPI.SubscriptionCase, async: false

  import WockyAPI.GraphQLHelper

  alias Ecto.Adapters.SQL.Sandbox

  setup_all do
    :ok = Sandbox.checkout(Wocky.Repo)
    Sandbox.mode(Wocky.Repo, {:shared, self()})
  end

  describe "bot access" do
    test "preallocate, update and get bot", %{
      socket: socket,
      token: token,
      user: %{id: user_id}
    } do
      authenticate(user_id, token, socket)
      create = "mutation { botCreate { successful, result { id } } }"

      ref! = push_doc(socket, create)

      assert_reply ref!,
                   :ok,
                   %{
                     data: %{
                       "botCreate" => %{
                         "successful" => true,
                         "result" => %{"id" => id}
                       }
                     }
                   },
                   150

      update = """
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

      ref! =
        push_doc(socket, update,
          variables: %{
            "id" => id,
            "values" => values
          }
        )

      assert_reply ref!, :ok, _, 150

      query = "query ($id: UUID!) { bot (id: $id) { title } }"

      ref! = push_doc(socket, query, variables: %{"id" => id})

      title = values[:title]

      assert_reply ref!, :ok, %{data: %{"bot" => %{"title" => ^title}}}, 150
    end
  end

  describe "current user refresh" do
    test "should not crash if no current user is set", %{socket: socket} do
      update = """
      mutation {
        userUpdate (input: {values: {transient: true}}) {
          successful
        }
      }
      """

      ref! = push_doc(socket, update)
      assert_reply ref!, :ok, _, 150
    end
  end
end
