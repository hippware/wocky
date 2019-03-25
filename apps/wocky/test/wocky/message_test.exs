defmodule Wocky.MessageTest do
  use Wocky.WatcherHelper

  import Ecto.Query

  alias Faker.{Code, Lorem}
  alias Pigeon.APNS.Notification
  alias Wocky.{Message, Push, Repo, Roster}
  alias Wocky.Push.Backend.Sandbox
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

  describe "send/4" do
    setup do
      [user, friend, stranger] = Factory.insert_list(3, :user)

      Roster.befriend(user, friend)

      {:ok, user: user, friend: friend, stranger: stranger}
    end

    test "should send a message to a friend", %{user: u, friend: f} do
      text = Lorem.paragraph()
      assert {:ok, _} = Message.send(f, u, text)

      assert [%Message{content: ^text}] =
               f |> Message.get_query(u) |> Repo.all()
    end

    test "should fail sending to an stranger", %{user: u, stranger: s} do
      assert {:error, :permission_denied} =
               Message.send(s, s, Lorem.paragraph())

      assert [] = s |> Message.get_query(u) |> Repo.all()
    end

    test "should fail sending to an non-existant user", %{user: u} do
      assert {:error, _} =
               Message.send(Factory.build(:user), u, Lorem.paragraph())
    end
  end

  describe "push notifications" do
    setup do
      user = Factory.insert(:user, device: "testing")
      Sandbox.clear_notifications(global: true)
      Push.enable(user, user.device, Code.isbn13())
      {:ok, user: user}
    end

    test "on message receipt", %{user: user} do
      text = Lorem.paragraph()
      m = Factory.insert(:message, content: text, recipient: user)

      msgs = Sandbox.wait_notifications(count: 1, timeout: 500, global: true)
      assert length(msgs) == 1

      assert %Notification{
               payload: %{
                 "aps" => %{"alert" => message}
               }
             } = hd(msgs)

      assert message == "From: @#{m.sender.handle}\n#{text}"
    end

    test "on image receipt", %{user: user} do
      text = Lorem.sentence()

      m =
        Factory.insert(:message, content: nil, image_url: text, recipient: user)

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
