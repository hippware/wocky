defmodule Wocky.ConversationSpec do
  use ESpec, async: true

  alias Wocky.Conversation
  alias Wocky.Repo.Factory
  alias Wocky.Repo.ID

  before do
    conversation = Factory.insert(:conversation)
    {:ok, conversation: conversation}
  end

  describe "find/1" do
    it "should return all conversation entries for a user" do
      conversations = Conversation.find(shared.conversation.user)
      conversations |> length |> should(eq 1)
      conversations |> hd |> should_match(shared.conversation)
    end

    it "should return an empty list if a user has no conversations" do
      Conversation.find(ID.new) |> should(eq [])
    end
  end

  describe "put/1" do
    context "when there is no existing entry for the other user" do
      before do
        conversation = Factory.build(:conversation)
        result = Conversation.put(conversation)
        {:ok,
         conversation: conversation,
         user: conversation.user,
         other: conversation.other_jid,
         result: result}
      end

      it "should return :ok" do
        shared.result |> should(eq :ok)
      end

      it "should create a new conversation entry" do
        conversations = Conversation.find(shared.user)
        conversations |> length |> should(eq 1)
        conversation = hd(conversations)
        should_match(conversation, shared.conversation)
      end
    end

    context "when there is an existing entry for the other user" do
      before do
        conversation = Factory.build(
                         :conversation,
                         %{user: shared.conversation.user,
                          other_jid: shared.conversation.other_jid})
        result = Conversation.put(conversation)
        {:ok,
         result: result,
         conversation: conversation,
         user: shared.conversation.user}
      end

      it "should return :ok" do
        shared.result |> should(eq :ok)
      end

      it "should replace the existing conversation entry" do
        conversations = Conversation.find(shared.user)
        length(conversations) |> should(eq 1)
        conversation = hd(conversations)
        should_match(conversation, shared.conversation)
      end

    end

  end

  defp should_match(a, b) do
    a.user      |> should(eq b.user)
    a.other_jid |> should(eq b.other_jid)
    a.message   |> should(eq b.message)
    a.outgoing  |> should(eq b.outgoing)
  end
end
