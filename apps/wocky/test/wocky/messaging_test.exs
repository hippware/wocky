defmodule Wocky.MessagingTest do
  use Wocky.DataCase

  alias Faker.Lorem
  alias Wocky.Messaging
  alias Wocky.Messaging.Conversation
  alias Wocky.Messaging.Message
  alias Wocky.Repo.Factory
  alias Wocky.Repo.ID
  alias Wocky.Roster

  describe "archive functions" do
    setup do
      [u1, u2, u3] = Factory.insert_list(3, :user)

      messages =
        5
        |> Factory.insert_list(:message, sender: u1, recipient: u2)
        |> normalise_messages()

      messages2 =
        3
        |> Factory.insert_list(:message, sender: u1, recipient: u3)
        |> normalise_messages()

      messages3 =
        3
        |> Factory.insert_list(:message, sender: u3, recipient: u1)
        |> normalise_messages()

      {:ok,
       user1: u1,
       user2: u2,
       user3: u3,
       messages: messages,
       messages2: messages2,
       messages3: messages3}
    end

    test "get_messages should work", %{
      user1: u1,
      messages: messages,
      messages2: messages2,
      messages3: messages3
    } do
      m =
        u1
        |> Messaging.get_messages_query()
        |> order_by(:created_at)
        |> Repo.all()
        |> normalise_messages()

      assert m == messages ++ messages2 ++ messages3
    end

    test "get_messages should work for a single user", %{
      user1: u1,
      user3: u3,
      messages2: messages2,
      messages3: messages3
    } do
      m =
        u1
        |> Messaging.get_messages_query(u3)
        |> order_by(:created_at)
        |> Repo.all()
        |> normalise_messages()

      assert m == messages2 ++ messages3
    end
  end

  describe "send_message/4" do
    setup do
      [user, friend, stranger] = Factory.insert_list(3, :user)

      Roster.befriend(user, friend)

      {:ok, user: user, friend: friend, stranger: stranger}
    end

    test "should send a message to a friend", %{user: u, friend: f} do
      text = Lorem.paragraph()
      assert {:ok, _} = Messaging.send_message(f, u, text)

      assert [%Message{content: ^text}] =
               f |> Messaging.get_messages_query(u) |> Repo.all()
    end

    test "should fail sending to an stranger", %{user: u, stranger: s} do
      assert {:error, :permission_denied} =
               Messaging.send_message(s, s, Lorem.paragraph())

      assert [] = s |> Messaging.get_messages_query(u) |> Repo.all()
    end

    test "should fail sending to an non-existant user", %{user: u} do
      assert {:error, _} =
               Messaging.send_message(
                 Factory.build(:user),
                 u,
                 Lorem.paragraph()
               )
    end
  end

  describe "get_conversations_query/1" do
    setup do
      # A simple user with one conversation
      user = Factory.insert(:user)

      conversations =
        for _ <- 1..10 do
          Factory.insert(:message, sender: user) |> to_conversation(user)
        end

      {:ok, user: user, conversations: conversations}
    end

    test "should return a query giving all conversations for a user", ctx do
      conversations =
        ctx.user.id
        |> Messaging.get_conversations_query()
        |> order_by(asc: :created_at)
        |> Repo.all()

      assert length(conversations) == 10

      conversations
      |> Enum.zip(ctx.conversations)
      |> Enum.all?(fn {a, b} -> assert_match(a, b) end)
    end

    test "should return an empty list if a user has no conversations" do
      assert ID.new() |> Messaging.get_conversations_query() |> Repo.all() == []
    end
  end

  defp assert_match(a, b) do
    fields = [:id, :user_id, :other_id, :message, :direction]
    assert Map.take(a, fields) == Map.take(b, fields)
  end

  defp to_conversation(message, user) do
    c =
      if message.sender.id == user.id do
        %Conversation{
          other_user: message.recipient,
          other_user_id: message.recipient_id,
          direction: :outgoing
        }
      else
        %Conversation{
          other_user: message.sender,
          other_user_id: message.sender_id,
          direction: :incoming
        }
      end

    %{
      c
      | id: message.id,
        content: message.content,
        image_url: message.image_url,
        user: user,
        user_id: user.id,
        created_at: message.created_at
    }
  end

  defp normalise_messages(messages) do
    messages
    |> Enum.map(&Repo.preload(&1, [:sender, :recipient]))
    |> Enum.map(&Map.drop(&1, [:__meta__]))
  end
end
