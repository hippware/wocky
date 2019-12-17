defmodule WockyAPI.GraphQL.MessageTest do
  use WockyAPI.GraphQLCase, async: true

  alias Faker.Lorem
  alias Wocky.Account
  alias Wocky.Contacts
  alias Wocky.Messaging
  alias Wocky.Messaging.Message
  alias Wocky.Repo
  alias Wocky.Repo.Factory
  alias Wocky.Repo.ID
  alias WockyAPI.Factory, as: APIFactory

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
              client_data
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
      user3: user3,
      messages: messages,
      messages2: messages2
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

      assert edges
             |> Enum.map(& &1["node"]["client_data"])
             |> Enum.uniq()
             |> Enum.sort() ==
               Enum.sort(Enum.map(messages ++ messages2, & &1.client_data))
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
                trosURL
                fullURL
                thumbnailURL
              }
              direction
              clientData
              updatedAt
              read
            }
          }
        }
      }
    }
    """

    test "get conversations", %{user: user, user2: user2} do
      image = Factory.insert(:tros_metadata, user: user)
      tros_url = APIFactory.image_url(image)

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
      first_name = Account.first_name(user2)
      content = message.content
      client_data = message.client_data

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
                           "trosURL" => ^tros_url,
                           "fullURL" => "https://" <> _,
                           "thumbnailURL" => "https://" <> _
                         },
                         "direction" => "OUTGOING",
                         "clientData" => ^client_data,
                         "read" => false,
                         "updatedAt" => _
                       }
                     }
                   ]
                 }
               }
             } = result.data
    end
  end

  describe "messageSend mutation" do
    @query """
    mutation ($recipientId: UUID!, $content: String!, $clientData: String!) {
      messageSend (input: {
          recipientId: $recipientId,
          content: $content,
          clientData: $clientData
        }) {
        result
      }
    }
    """

    test "should send a message to the specified user", %{
      user: user,
      user2: user2
    } do
      text = Lorem.paragraph()
      data = Lorem.paragraph()
      Contacts.befriend(user, user2)

      result =
        run_query(@query, user, %{
          "recipientId" => user2.id,
          "content" => text,
          "clientData" => data
        })

      refute has_errors(result)

      assert [%Message{content: ^text, client_data: ^data}] =
               user2 |> Messaging.get_messages_query() |> Repo.all()
    end
  end

  describe "mark read mutation" do
    @query """
    mutation ($messageMarkReadInput: MessageMarkReadInput) {
      messageMarkRead (input: $messageMarkReadInput) {
        result {
          id
          successful
          error
        }

      }
    }
    """

    setup ctx do
      unread = Factory.insert(:message, sender: ctx.user, recipient: ctx.user2)

      read =
        Factory.insert(:message,
          sender: ctx.user,
          recipient: ctx.user2,
          read: true
        )

      {:ok, unread: unread, read: read}
    end

    test "should mark a message as read", ctx do
      result =
        run_query(@query, ctx.user2, %{
          "messageMarkReadInput" => %{"messages" => [%{"id" => ctx.unread.id}]}
        })

      refute has_errors(result)

      assert result.data == %{
               "messageMarkRead" => %{
                 "result" => [
                   %{
                     "error" => nil,
                     "successful" => true,
                     "id" => ctx.unread.id
                   }
                 ]
               }
             }

      assert Repo.get(Message, ctx.unread.id).read
    end

    test "should refuse to mark a message not sent to the requesting user",
         ctx do
      result =
        run_query(@query, ctx.user, %{
          "messageMarkReadInput" => %{"messages" => [%{"id" => ctx.unread.id}]}
        })

      refute has_errors(result)

      assert result.data == %{
               "messageMarkRead" => %{
                 "result" => [
                   %{
                     "error" => "Invalid message ID",
                     "successful" => false,
                     "id" => ctx.unread.id
                   }
                 ]
               }
             }

      refute Repo.get(Message, ctx.unread.id).read
    end

    test "should work for multiple messages with different read flags", ctx do
      result =
        run_query(@query, ctx.user2, %{
          "messageMarkReadInput" => %{
            "messages" => [
              %{"id" => ctx.unread.id},
              %{"id" => ctx.read.id, "read" => false}
            ]
          }
        })

      refute has_errors(result)

      assert Repo.get(Message, ctx.unread.id).read
      refute Repo.get(Message, ctx.read.id).read
    end
  end
end
