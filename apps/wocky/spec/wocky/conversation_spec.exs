defmodule Wocky.ConversationSpec do
  use ESpec, async: true

  alias Wocky.Conversation
  alias Wocky.JID
  alias Wocky.Repo.Factory
  alias Wocky.Repo.ID
  alias Wocky.User

  before do
    # A simple user with one conversation
    user = Factory.insert(:user, %{server: shared.server})
    conversation = Factory.insert(:conversation, user: user)

    # A user with 10 conversations
    user2 = Factory.insert(:user, %{server: shared.server})
    conversations = for _ <- 1..10 do
      Factory.insert(:conversation, user: user2)
    end

    {:ok,
      user: user,
      user2: user2,
      conversation: conversation,
      conversations: conversations
    }
  end

  describe "find/1" do
    it "should return all conversation entries for a user" do
      conversations = Conversation.find(shared.conversation.user_id)
      conversations |> should(have_length 1)
      conversations |> hd |> should_match(shared.conversation)
    end

    it "should return conversations sorted by decreasing timestamp" do
      conversations = Conversation.find(shared.user2.id)
      conversations |> should(have_length 10)
      Enum.zip(conversations, Enum.reverse(shared.conversations))
      |> should(have_all fn({a, b}) -> should_match(a, b) end)
    end

    it "should return an empty list if a user has no conversations" do
      Conversation.find(ID.new) |> should(eq [])
    end
  end

  describe "get_id/2" do
    context "when the conversation exists" do
      before do
        other_jid = shared.conversation.other_jid
        result = Conversation.get_id(shared.user.id, other_jid)
        {:ok, result: result}
      end

      it "should return an integer" do
        shared.result |> should(be_integer())
      end

      it "should return the conversation id" do
        shared.result |> should(eq shared.conversation.id)
      end
    end

    context "when the conversation does not exist" do
      it "should return nil" do
        Conversation.get_id(shared.user.id, Factory.new_jid)
        |> should(be_nil())
      end
    end
  end

  describe "put/4" do
    context "when there is no existing entry for the other user" do
      before do
        user = Factory.insert(:user, %{server: shared.server})
        conversation = Factory.build(:conversation, user_id: user.id)
        result = Conversation.put(conversation.id,
                                  user.id,
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

        result = Conversation.put(conversation.id,
                                  conversation.user_id,
                                  conversation.other_jid,
                                  conversation.message,
                                  conversation.outgoing)
        {:ok,
         result: result,
         new_conversation: conversation,
         user_id: shared.conversation.user_id}
      end

      it "should return :ok" do
        shared.result |> should(eq :ok)
      end

      it "should replace the existing conversation entry" do
        conversations = Conversation.find(shared.user_id)
        conversations |> should(have_length 1)

        c = hd(conversations)
        c |> should_match(shared.new_conversation, [:id])

        c.updated_at
        |> DateTime.compare(shared.conversation.updated_at)
        |> should(eq :gt)

        c.id |> should(eq shared.conversation.id)
      end

    end

  end

  describe "delete_user_pair/2" do
    before do
      conversation =
        Factory.insert(:conversation,
                       %{user: shared.user,
                         other_jid: JID.to_binary(User.to_jid(shared.user2))})
      {:ok, conversation: conversation}
    end

    it "should return :ok" do
      Conversation.delete_user_pair(shared.user, shared.user2) |> should(eq :ok)
    end

    it "should remove all entries for the user pair" do
      Conversation.delete_user_pair(shared.user, shared.user2)
      Conversation.find(shared.user.id)
      |> should_not(have(shared.conversation))
    end
  end

  defp should_match(a, b, exclusions \\ []) do
    fields = [:id, :user_id, :other_jid, :message, :outgoing] -- exclusions
    a |> Map.take(fields) |> should(eq b |> Map.take(fields))
  end
end
