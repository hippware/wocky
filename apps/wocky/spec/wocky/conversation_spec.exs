defmodule Wocky.ConversationSpec do
  use ESpec, async: true

  alias Wocky.Conversation
  alias Wocky.Repo.Factory
  alias Wocky.Repo.ID

  before do
    # A simple user with one conversation
    user = Factory.insert(:user, %{server: shared.server})
    conversation = Factory.insert(:conversation, user: user)

    # A user with 10 conversations
    user2 = Factory.insert(:user, %{server: shared.server})
    conversations = for _ <- 1..10 do
      Factory.insert(:conversation, user: user2)
    end

    {:ok, conversation: conversation,
     conversations: conversations, user2: user2}
  end

  describe "find/1" do
    it "should return all conversation entries for a user" do
      conversations = Conversation.find(shared.conversation.user_id)
      conversations |> should(have_length 1)
      conversations |> hd |> should_match(shared.conversation)
    end

    it "should return conversations sorted by timestamp" do
      conversations = Conversation.find(shared.user2.id)
      conversations |> should(have_length 10)
      Enum.zip(conversations, shared.conversations)
      |> should(have_all fn({a, b}) -> should_match(a, b) end)
    end

    it "should return an empty list if a user has no conversations" do
      Conversation.find(ID.new) |> should(eq [])
    end
  end

  describe "put/4" do
    context "when there is no existing entry for the other user" do
      before do
        user = Factory.insert(:user, %{server: shared.server})
        conversation = Factory.build(:conversation, user_id: user.id)
        result = Conversation.put(user.id,
                                  conversation.other_jid,
                                  conversation.message,
                                  conversation.outgoing)
        {:ok,
         conversation: conversation,
         user: user,
         other: conversation.other_jid,
         result: result}
      end

      it "should return :ok" do
        shared.result |> should(eq :ok)
      end

      it "should create a new conversation entry" do
        conversations = Conversation.find(shared.user.id)
        conversations |> should(have_length 1)
        conversations |> hd |> should_match(shared.conversation)
      end
    end

    context "when there is an existing entry for the other user" do
      before do
        conversation = Factory.build(
                         :conversation,
                         %{user_id: shared.conversation.user_id,
                          other_jid: shared.conversation.other_jid})
        result = Conversation.put(conversation.user_id,
                                  conversation.other_jid,
                                  conversation.message,
                                  conversation.outgoing)
        {:ok,
         result: result,
         conversation: conversation,
         user_id: shared.conversation.user_id}
      end

      it "should return :ok" do
        shared.result |> should(eq :ok)
      end

      it "should replace the existing conversation entry" do
        conversations = Conversation.find(shared.user_id)
        conversations |> should(have_length 1)
        conversations |> hd |> should_match(shared.conversation)
      end

    end

  end

  defp should_match(a, b) do
    fields = [:user_id, :other_jid, :message, :outgoing]
    a |> Map.take(fields) |> should(eq b |> Map.take(fields))
  end
end
