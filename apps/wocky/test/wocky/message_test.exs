defmodule Wocky.MessageTest do
  use Wocky.WatcherHelper

  import Ecto.Query

  alias Faker.{Code, Lorem}
  alias Pigeon.APNS.Notification
  alias Wocky.{Message, Push, Repo, Roster}
  alias Wocky.Push.Sandbox
  alias Wocky.Repo.Factory

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
        |> Message.get_query()
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
        |> Message.get_query(u3)
        |> order_by(:created_at)
        |> Repo.all()
        |> normalise_messages()

      assert m == messages2 ++ messages3
    end
  end

  describe "send/3" do
    setup do
      [user, follower, friend, followee, stranger] =
        Factory.insert_list(5, :user)

      Roster.befriend(user.id, friend.id)
      Roster.follow(follower.id, user.id)
      Roster.follow(user.id, followee.id)

      {:ok,
       user: user,
       friend: friend,
       follower: follower,
       followee: followee,
       stranger: stranger}
    end

    test "should send a message to a friend", %{user: u, friend: f} do
      text = Lorem.paragraph()
      assert {:ok, _} = Message.send(text, f, u)

      assert [%Message{message: ^text}] =
               f |> Message.get_query(u) |> Repo.all()
    end

    test "should send a message to a follower", %{user: u, follower: f} do
      text = Lorem.paragraph()
      assert {:ok, _} = Message.send(text, f, u)

      assert [%Message{message: ^text}] =
               f |> Message.get_query(u) |> Repo.all()
    end

    test "should fail sending to an followee", %{user: u, followee: f} do
      assert {:error, :permission_denied} =
               Message.send(Lorem.paragraph(), f, u)

      assert [] = f |> Message.get_query(u) |> Repo.all()
    end

    test "should fail sending to an stranger", %{user: u, stranger: s} do
      assert {:error, :permission_denied} =
               Message.send(Lorem.paragraph(), s, s)

      assert [] = s |> Message.get_query(u) |> Repo.all()
    end

    test "should fail sending to an non-existant user", %{user: u} do
      assert {:error, _} =
               Message.send(Lorem.paragraph(), Factory.build(:user), u)
    end
  end

  describe "get_body/1" do
    test "extract a body if one exists" do
      assert "body value" ==
               %Message{message: "<tag>afdsa</tag><body>body value</body>"}
               |> Message.get_body()
    end

    test "return an empty string if no body exists" do
      assert "" ==
               %Message{message: "<tag>blah</tag>"}
               |> Message.get_body()
    end

    test "return nil for non-XML" do
      assert nil ==
               %Message{message: "<bodyyyyyy>body value</body>"}
               |> Message.get_body()
    end
  end

  describe "get_image/1" do
    test "extract an image if one exists" do
      assert "http://image.com" ==
               %Message{
                 message:
                   "<tag>afdsa</tag><image><url>http://image.com</url></image><other>xxx</other>"
               }
               |> Message.get_image()
    end

    test "return an empty string if no image exists" do
      assert "" ==
               %Message{message: "<tag>blah</tag>"}
               |> Message.get_image()
    end

    test "return nil for non-XML" do
      assert nil ==
               %Message{message: "<image><url>body value</url></imasdfs>"}
               |> Message.get_image()
    end
  end

  describe "push notifications" do
    setup do
      user = Factory.insert(:user, resource: "testing")
      Sandbox.clear_notifications(global: true)
      Push.enable(user.id, user.resource, Code.isbn13())
      {:ok, user: user}
    end

    test "on message receipt", %{user: user} do
      body = Lorem.paragraph()
      text = "<body>" <> body <> "</body>"
      m = Factory.insert(:message, message: text, recipient: user)

      msgs = Sandbox.wait_notifications(count: 1, timeout: 500, global: true)
      assert length(msgs) == 1

      assert %Notification{
               payload: %{
                 "aps" => %{"alert" => message}
               }
             } = hd(msgs)

      assert message == "From: @#{m.sender.handle}\n#{body}"
    end

    test "on image receipt", %{user: user} do
      body = Lorem.paragraph()
      text = "<image>" <> body <> "</image>"
      m = Factory.insert(:message, message: text, recipient: user)

      msgs = Sandbox.wait_notifications(count: 1, timeout: 500, global: true)
      assert length(msgs) == 1

      assert %Notification{
               payload: %{
                 "aps" => %{"alert" => message}
               }
             } = hd(msgs)

      assert message == "@#{m.sender.handle} sent you an image!"
    end
  end

  defp normalise_messages(messages) do
    messages
    |> Enum.map(&Repo.preload(&1, [:sender, :recipient]))
    |> Enum.map(&Map.drop(&1, [:__meta__]))
  end
end
