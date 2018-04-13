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
        Collections.update(id, new_title, ctx.user)
    end

    test "delete a collection", ctx do
     {:ok, %Collection{id: id}} = ctx.result

      assert {:ok, %Collection{}} = Collections.delete(id, ctx.user)
      assert nil == Repo.get(Collection, id)
    end

    test "delete a non-existant collection", ctx do
      assert {:ok, nil} = Collections.delete(:rand.uniform(100), ctx.user)
    end
  end

  describe "bot addition and removal" do
    setup %{user: user} do
      {:ok,
        collection: Factory.insert(:collection, user: user),
        bots: Factory.insert_list(10, :bot, public: true)}
    end

    test "add bots to a collection", ctx do
      for bot <- ctx.bots do
        bot_id = bot.id
        collection_id = ctx.collection.id
        assert {:ok, %Member{collection_id: ^collection_id, bot_id: ^bot_id}} =
          Collections.add_bot(collection_id, bot_id, ctx.user)
      end

      members = Collection.bots_query(ctx.collection) |> Repo.all()

      assert ids(members) == ids(ctx.bots)
    end

    test "remove bots from a collection", ctx do
      collection_id = ctx.collection.id

      for bot <- ctx.bots do
        bot_id = bot.id
        assert {:ok, %Member{collection_id: ^collection_id, bot_id: ^bot_id}} =
          Collections.add_bot(collection_id, bot_id, ctx.user)
      end

      for bot <- Enum.slice(ctx.bots, 0..4) do
        bot_id = bot.id
        assert {:ok, %Member{collection_id: ^collection_id, bot_id: ^bot_id}} =
          Collections.remove_bot(collection_id, bot_id, ctx.user)
      end

      remaining = Collection.bots_query(ctx.collection) |> Repo.all()
      assert ids(remaining) == ids(Enum.slice(ctx.bots, 5..9))
    end
  end


  describe "user (un)subscription" do
    setup do
      {:ok,
        collection: Factory.insert(:collection),
        users: Factory.insert_list(10, :user)}
    end

    test "subscribe to a collection", ctx do
      for user <- ctx.users do
        user_id = user.id
        collection_id = ctx.collection.id
        assert {:ok,
          %Subscription{collection_id: ^collection_id, user_id: ^user_id}} =
            Collections.subscribe(collection_id, user)
      end

      members = Collection.subscribers_query(ctx.collection) |> Repo.all()

      assert ids(members) == ids(ctx.users)
    end

    test "unsubscribe from a collection", ctx do
      collection_id = ctx.collection.id

      for user <- ctx.users do
        user_id = user.id
        assert {:ok,
          %Subscription{collection_id: ^collection_id, user_id: ^user_id}} =
            Collections.subscribe(collection_id, user)
      end

      for user <- Enum.slice(ctx.users, 0..4) do
        user_id = user.id
        assert {:ok,
          %Subscription{collection_id: ^collection_id, user_id: ^user_id}} =
            Collections.unsubscribe(collection_id, user)
      end

      remaining = Collection.subscribers_query(ctx.collection) |> Repo.all()
      assert ids(remaining) == ids(Enum.slice(ctx.users, 5..9))
    end
  end

  defp ids(items), do: Enum.sort(Enum.map(items, &(&1.id)))
end
