defmodule Wocky.ConversationTest do
  use Wocky.DataCase, async: true

  alias Wocky.Conversation
  alias Wocky.JID
  alias Wocky.Repo.Factory
  alias Wocky.Repo.ID
  alias Wocky.User

  setup do
    # A simple user with one conversation
    user = Factory.insert(:user)
    conversation = Factory.insert(:conversation, user: user)

    # A user with 10 conversations
    user2 = Factory.insert(:user)

    conversations =
      for _ <- 1..10 do
        Factory.insert(:conversation, user: user2)
      end

    {:ok,
     user: user,
     user2: user2,
     conversation: conversation,
     conversations: conversations}
  end

  describe "find/1" do
    test "should return all conversation entries for a user", ctx do
      conversations = Conversation.find(ctx.conversation.user_id)
      assert length(conversations) == 1

      assert_match(hd(conversations), ctx.conversation)
    end

    test "should return conversations sorted by decreasing timestamp", ctx do
      conversations = Conversation.find(ctx.user2.id)
      assert length(conversations) == 10

      conversations
      |> Enum.zip(Enum.reverse(ctx.conversations))
      |> Enum.all?(fn {a, b} -> assert_match(a, b) end)
    end

    test "should return an empty list if a user has no conversations" do
      assert Conversation.find(ID.new()) == []
    end
  end

  describe "get_id/2" do
    setup ctx do
      other_jid = ctx.conversation.other_jid
      result = Conversation.get_id(ctx.user.id, other_jid)
      {:ok, result: result}
    end

    test "when the conversation exists", ctx do
      assert is_integer(ctx.result)
      assert ctx.result == ctx.conversation.id
    end

    test "when the conversation does not exist", ctx do
      refute Conversation.get_id(ctx.user.id, Factory.new_jid())
    end
  end

  describe "put/4 when there is no existing entry for the other user" do
    setup do
      user = Factory.insert(:user)
      conversation = Factory.build(:conversation, user_id: user.id)

      result =
        Conversation.put(
          conversation.id,
          user.id,
          conversation.other_jid,
          conversation.message,
          conversation.outgoing
        )

      {:ok,
       conversation: conversation,
       user: user,
       other: conversation.other_jid,
       result: result}
    end

    test "should create a new conversation entry", ctx do
      assert ctx.result == :ok

      conversations = Conversation.find(ctx.user.id)
      assert length(conversations) == 1

      assert_match(hd(conversations), ctx.conversation)
    end
  end

  describe "put/4 when there is an existing entry for the other user" do
    setup ctx do
      conversation =
        Factory.build(:conversation, %{
          user_id: ctx.conversation.user_id,
          other_jid: ctx.conversation.other_jid
        })

      result =
        Conversation.put(
          conversation.id,
          conversation.user_id,
          conversation.other_jid,
          conversation.message,
          conversation.outgoing
        )

      {:ok,
       result: result,
       new_conversation: conversation,
       user_id: ctx.conversation.user_id}
    end

    test "should replace the existing conversation entry", ctx do
      assert ctx.result == :ok

      conversations = Conversation.find(ctx.user_id)
      assert length(conversations) == 1

      c = hd(conversations)
      assert_match(c, ctx.new_conversation, [:id])

      assert DateTime.compare(c.updated_at, ctx.conversation.updated_at) == :gt
      assert c.id == ctx.conversation.id
    end
  end

  describe "delete_user_pair/2" do
    setup ctx do
      conversation =
        Factory.insert(:conversation, %{
          user: ctx.user,
          other_jid: JID.to_binary(User.to_jid(ctx.user2))
        })

      {:ok, conversation: conversation}
    end

    test "should remove all entries for the user pair", ctx do
      assert Conversation.delete_user_pair(ctx.user, ctx.user2) == :ok

      refute ctx.user.id
             |> Conversation.find()
             |> Enum.member?(ctx.conversation)
    end
  end

  defp assert_match(a, b, exclusions \\ []) do
    fields = [:id, :user_id, :other_jid, :message, :outgoing] -- exclusions
    assert Map.take(a, fields) == Map.take(b, fields)
  end
end
