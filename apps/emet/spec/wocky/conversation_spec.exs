defmodule Wocky.ConversationSpec do
  use ESpec, async: true
  use Wocky.Ejabberd

  alias Golem.ID
  alias Wocky.Conversation

  before do
    add_entry(shared)
  end

  describe "new/7" do
    it "should return a Conversation struct" do
      shared.conversation |> should(be_struct Conversation)
    end

    it "should include the passed id" do
      shared.conversation.id |> should(eq shared.id)
    end

    it "should include the passed server" do
      shared.conversation.server |> should(eq shared.server)
    end

    it "should include passed data in struct" do
      shared.conversation.message |> should(eq shared.message)
    end
  end

  describe "find/1" do
    it "should return all conversation entries for a user" do
      conversations = Conversation.find(shared.user)
      length(conversations) |> should(eq 1)
      hd(conversations) |> should(eq shared.conversation)
    end

    it "should return an empty list if a user has no conversations" do
      Conversation.find(ID.new) |> should(eq [])
    end
  end

  describe "put/1" do
    context "when there is no existing entry for the other user" do
      before do
        add_entry(shared)
      end

      it "should return :ok" do
        shared.result |> should(eq :ok)
      end

      it "should create a new conversation entry" do
        conversations = Conversation.find(shared.user)
        length(conversations) |> should(eq 1)
        hd(conversations) |> should(eq shared.conversation)
      end
    end

    context "when there is an existing entry for the other user" do
      before do
        add_entry(shared.user, shared.other, shared)
      end

      it "should return :ok" do
        shared.result |> should(eq :ok)
      end

      it "should replace the existing conversation entry" do
        conversations = Conversation.find(shared.user)
        length(conversations) |> should(eq 1)
        hd(conversations) |> should(eq shared.conversation)
      end

    end

  end

  defp add_entry(shared) do
    user = ID.new
    other = ID.new
    add_entry(user, other, shared)
  end

  defp add_entry(user, other, shared) do
    message_obj = xmlel(name: "message",
                        children: [
                          xmlcdata(content: Faker.Lorem.sentence())])
    message = :exml.to_binary(message_obj)
    id = :rand.uniform(100000000)
    conversation = Conversation.new(id,
                                    shared.server,
                                    user,
                                    other,
                                    :rand.uniform(100000000),
                                    message,
                                    true
                                   )
    result = Conversation.put(conversation)
    # Let solr catch up
    :timer.sleep(1000)
    {:ok, user: user, id: id, result: result, user: user,
     other: other, message: message, conversation: conversation}
  end

end
