defmodule Wocky.ConversationTest do
  use Wocky.DataCase, async: true

  import Ecto.Query

  alias Wocky.Conversation
  alias Wocky.Repo.Factory
  alias Wocky.Repo.ID

  setup do
    # A simple user with one conversation
    user = Factory.insert(:user)

    conversations =
      for _ <- 1..10 do
        Factory.insert(:message, sender: user) |> to_conversation(user)
      end

    {:ok, user: user, conversations: conversations}
  end

  describe "by_user_query/1" do
    test "should return a query giving all conversations for a user", ctx do
      conversations =
        ctx.user.id
        |> Conversation.by_user_query()
        |> order_by(asc: :created_at)
        |> Repo.all()

      assert length(conversations) == 10

      conversations
      |> Enum.zip(ctx.conversations)
      |> Enum.all?(fn {a, b} -> assert_match(a, b) end)
    end

    test "should return an empty list if a user has no conversations" do
      assert ID.new() |> Conversation.by_user_query() |> Repo.all() == []
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
        message: message.message,
        user: user,
        user_id: user.id,
        created_at: message.created_at
    }
  end
end
