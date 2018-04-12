defmodule Wocky.CollectionsTest do
  use Wocky.DataCase

  alias Faker.Lorem
  alias Wocky.Collections
  alias Wocky.Collections.Collection
  alias Wocky.Collections.Member
  alias Wocky.Collections.Subscription
  alias Wocky.Repo.Factory

  setup do
    {:ok, user: Factory.insert(:user)}
  end

  describe "Basic collections operations" do
    setup %{user: user} do
      title = Lorem.sentence()
      result = Collections.create(title, user)

      {:ok, title: title, result: result}
    end

    test "create a collection", %{user: %{id: user_id}, title: title} = ctx do
      assert {:ok, %Collection{id: id, title: ^title}} = ctx.result

      assert %Collection{title: ^title, user_id: ^user_id} =
        Repo.get(Collection, id)
    end

    test "update a collection", ctx do
      {:ok, %Collection{id: id}} = ctx.result

      new_title = Lorem.sentence()
      assert {:ok, %Collection{id: ^id, title: ^new_title}} =
        Collections.update(id, new_title)
    end

    test "delete a collection", ctx do
     {:ok, %Collection{id: id}} = ctx.result

      assert :ok == Collections.delete(id)
      assert nil == Repo.get(Collection, id)
    end

    test "delete a non-existant collection" do
      assert :ok = Collections.delete(:rand.uniform(100))
    end
  end

  describe "bot addition and removal" do
    setup do
      {:ok,
        collection: Factory.insert(:collection),
        bots: Factory.insert_list(10, :bot, public: true)}
    end

    test "add bots to a collection",
    %{collection: %{id: collection_id} = collection, bots: bots} do
      for bot <- bots do
        bot_id = bot.id
        assert {:ok, %Member{collection_id: ^collection_id, bot_id: ^bot_id}} =
          Collections.add_bot(collection_id, bot_id)
      end

      members = Collection.bots_query(collection) |> Repo.all()

      assert ids(members) == ids(bots)
    end

    test "remove bots from a collection",
    %{collection: %{id: collection_id} = collection, bots: bots} do
      for bot <- bots do
        bot_id = bot.id
        assert {:ok, %Member{collection_id: ^collection_id, bot_id: ^bot_id}} =
          Collections.add_bot(collection_id, bot_id)
      end

      for bot <- Enum.slice(bots, 0..4) do
        assert :ok == Collections.remove_bot(collection_id, bot.id)
      end

      remaining = Collection.bots_query(collection) |> Repo.all()
      assert ids(remaining) == ids(Enum.slice(bots, 5..9))
    end
  end


  describe "user (un)subscription" do
    setup do
      {:ok,
        collection: Factory.insert(:collection),
        users: Factory.insert_list(10, :user)}
    end

    test "subscribe to a collection",
    %{collection: %{id: collection_id} = collection, users: users} do
      for user <- users do
        user_id = user.id
        assert {:ok,
          %Subscription{collection_id: ^collection_id, user_id: ^user_id}} =
            Collections.subscribe(collection_id, user_id)
      end

      members = Collection.subscribers_query(collection) |> Repo.all()

      assert ids(members) == ids(users)
    end

    test "unsubscribe from a collection",
    %{collection: %{id: collection_id} = collection, users: users} do
      for user <- users do
        user_id = user.id
        assert {:ok,
          %Subscription{collection_id: ^collection_id, user_id: ^user_id}} =
            Collections.subscribe(collection_id, user_id)
      end

      for user <- Enum.slice(users, 0..4) do
        assert :ok == Collections.unsubscribe(collection_id, user.id)
      end

      remaining = Collection.subscribers_query(collection) |> Repo.all()
      assert ids(remaining) == ids(Enum.slice(users, 5..9))
    end
  end

  defp ids(items), do: Enum.sort(Enum.map(items, &(&1.id)))
end
