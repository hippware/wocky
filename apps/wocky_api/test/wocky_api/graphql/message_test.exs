defmodule WockyAPI.GraphQL.MessageTest do
  use WockyAPI.GraphQLCase, async: true

  alias Faker.Lorem
  alias Wocky.{Message, Repo, Roster, User}
  alias Wocky.Repo.{Factory, ID}

  setup do
    [user, user2] = Factory.insert_list(2, :user)

    {:ok, user: user, user2: user2}
  end

  describe "message archive" do
    @query """
    query ($other_user: UUID) {
      currentUser {
        messages (other_user: $other_user, first: 50){
          total_count
          edges {
            node {
              id
              other_user {
                id
                handle
              }
              content
            }
          }
        }
      }
    }
    """
    setup shared do
      user3 = Factory.insert(:user)

      messages =
        Factory.insert_list(5, :message,
          sender: shared.user,
          recipient: shared.user2
        )

      messages2 =
        Factory.insert_list(3, :message, sender: shared.user, recipient: user3)

      {:ok, user3: user3, messages: messages, messages2: messages2}
    end

    test "should successfully retrieve all messages", %{
      user: user,
      user2: user2,
      user3: user3
    } do
      result = run_query(@query, user)

      refute has_errors(result)

      assert %{
               "currentUser" => %{
                 "messages" => %{
                   "total_count" => 8,
                   "edges" => edges
                 }
               }
             } = result.data

      assert length(edges) == 8

      assert edges
             |> Enum.map(& &1["node"]["other_user"]["id"])
             |> Enum.uniq()
             |> Enum.sort() == Enum.sort([user2.id, user3.id])

      assert edges
             |> Enum.map(& &1["node"]["other_user"]["handle"])
             |> Enum.uniq()
             |> Enum.sort() == Enum.sort([user2.handle, user3.handle])
    end

    test "should retrieve all messages for a given user", %{
      user: user,
      user2: user2
    } do
      result = run_query(@query, user, %{"other_user" => user2.id})

      refute has_errors(result)

      assert %{
               "currentUser" => %{
                 "messages" => %{
                   "total_count" => 5,
                   "edges" => edges
                 }
               }
             } = result.data

      assert length(edges) == 5

      assert edges |> Enum.map(& &1["node"]["other_user"]["id"]) |> Enum.uniq() ==
               [user2.id]

      assert edges
             |> Enum.map(& &1["node"]["other_user"]["handle"])
             |> Enum.uniq() == [user2.handle]
    end

    test "should return an error for a non-existant user", %{user: user} do
      result = run_query(@query, user, %{"other_user" => ID.new()})

      assert has_errors(result)

      assert error_msg(result) =~ "User not found"
    end
  end

  describe "conversations" do
    @query """
    {
      currentUser {
        conversations (first: 1) {
          totalCount
          edges {
            node {
              otherUser {
                id
                firstName
              }
              content
              media {
                tros_url
                full_url
                thumbnail_url
              }
              direction
            }
          }
        }
      }
    }
    """

    test "get conversations", %{user: user, user2: user2} do
      image = Factory.insert(:tros_metadata, user: user)
      tros_url = Factory.image_url(image)

      message =
        Factory.insert(
          :message,
          sender: user,
          recipient: user2,
          image_url: tros_url
        )

      result = run_query(@query, user)

      refute has_errors(result)

      user_id = user2.id
      first_name = User.first_name(user2)
      content = message.content

      assert %{
               "currentUser" => %{
                 "conversations" => %{
                   "totalCount" => 1,
                   "edges" => [
                     %{
                       "node" => %{
                         "otherUser" => %{
                           "id" => ^user_id,
                           "firstName" => ^first_name
                         },
                         "content" => ^content,
                         "media" => %{
                           "tros_url" => ^tros_url,
                           "full_url" => "https://" <> _,
                           "thumbnail_url" => "https://" <> _
                         },
                         "direction" => "OUTGOING"
                       }
                     }
                   ]
                 }
               }
             } = result.data
    end
  end

  describe "send message mutation" do
    @query """
    mutation ($recipientId: UUID!, $content: String!) {
      sendMessage (input: {recipientId: $recipientId, content: $content}) {
        result
      }
    }
    """

    test "should send a message to the specified user", %{
      user: user,
      user2: user2
    } do
      text = Lorem.paragraph()
      Roster.befriend(user, user2)

      result =
        run_query(@query, user, %{"recipientId" => user2.id, "content" => text})

      refute has_errors(result)

      assert [%Message{content: ^text}] =
               user2 |> Message.get_query() |> Repo.all()
    end
  end
end
