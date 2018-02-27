defmodule Wocky.HomeStreamSpec do
  use ESpec, async: true
  use ModelHelpers

  alias Faker.Lorem
  alias Wocky.HomeStream
  alias Wocky.HomeStream.Item, as: HomeStreamItem
  alias Wocky.Repo.Factory
  alias Wocky.Repo.ID

  @num_items 10

  before do
    user = Factory.insert(:user, %{server: shared.server})
    bot = Factory.insert(:bot, %{user: user})

    items =
      for _ <- 1..@num_items do
        Factory.insert(:home_stream_item, %{user: user})
      end

    # ref_update objects should not be returned by standard get operations
    # They are only sent as async updates
    update_item =
      Factory.insert(:home_stream_item, %{
        user: user,
        class: :ref_update,
        reference_bot: bot
      })

    middle_item = div(@num_items, 2) - 1

    {:ok,
     user: user,
     items: items,
     middle_time: Enum.at(items, middle_item).updated_at,
     after_items: Enum.slice(items, (middle_item + 1)..@num_items),
     last_item: List.last(items),
     update_item: update_item,
     version: update_item.updated_at}
  end

  describe "put/4" do
    context "when there is no existing record for the key" do
      it "should insert a new record" do
        key = Factory.new_jid()
        from_jid = Factory.new_jid()
        stanza = Lorem.paragraph()
        put_result = HomeStream.put(shared.user.id, key, from_jid, stanza)
        put_result |> should(be_ok_result())
        put_result |> Kernel.elem(1) |> should(be_struct HomeStreamItem)

        get_result = HomeStream.get_by_key(shared.user.id, key)
        get_result |> should(be_struct HomeStreamItem)
        get_result.user_id |> should(eq shared.user.id)
        get_result.key |> should(eq key)
        get_result.from_jid |> should(eq from_jid)
        get_result.stanza |> should(eq stanza)
        get_result.class |> should(eq :item)
      end
    end

    context "when there is an existing record for the key" do
      it "should overwrite the record" do
        stanza = Lorem.paragraph()

        put_result =
          HomeStream.put(
            shared.user.id,
            shared.last_item.key,
            shared.last_item.from_jid,
            stanza
          )

        put_result |> should(be_ok_result())
        put_result |> Kernel.elem(1) |> should(be_struct HomeStreamItem)

        get_result = HomeStream.get_by_key(shared.user.id, shared.last_item.key)

        get_result |> should(be_struct HomeStreamItem)
        get_result.stanza |> should(eq stanza)
      end
    end

    context "with referenced user or bot" do
      it "should store the reference" do
        ref_user = Factory.insert(:user)
        ref_bot = Factory.insert(:bot)

        put_result =
          HomeStream.put(
            shared.user.id,
            shared.last_item.key,
            shared.last_item.from_jid,
            Lorem.paragraph(),
            ref_user_id: ref_user.id,
            ref_bot_id: ref_bot.id
          )

        put_result |> Kernel.elem(1) |> should(be_struct HomeStreamItem)

        get_result = HomeStream.get_by_key(shared.user.id, shared.last_item.key)

        get_result |> should(be_struct HomeStreamItem)
        get_result.reference_user_id |> should(eq ref_user.id)
        get_result.reference_bot_id |> should(eq ref_bot.id)
      end

      it "should fail on invalid references" do
        HomeStream.put(
          shared.user.id,
          shared.last_item.key,
          shared.last_item.from_jid,
          Lorem.paragraph(),
          ref_user_id: ID.new()
        )
        |> should(be_error_result())

        HomeStream.put(
          shared.user.id,
          shared.last_item.key,
          shared.last_item.from_jid,
          Lorem.paragraph(),
          ref_bot_id: ID.new()
        )
        |> should(be_error_result())
      end
    end

    context "when the user is invalid" do
      it "should cause an exception" do
        fn ->
          HomeStream.put(
            ID.new(),
            Lorem.word(),
            Factory.make_jid(),
            Lorem.paragraph()
          )
        end
        |> should(raise_exception())
      end
    end
  end

  describe "delete/2" do
    it "should flag an existing item as deleted" do
      result = HomeStream.delete(shared.user.id, shared.last_item.key)
      result |> should(be_ok_result())
      result_val = Kernel.elem(result, 1)
      result_val |> should(be_struct HomeStreamItem)
      result_val.class |> should(eq :deleted)
    end

    it "should not fail for a non-existant item" do
      shared.user.id
      |> HomeStream.delete(Factory.new_jid())
      |> should(eq {:ok, nil})
    end
  end

  describe "deletion by reference" do
    before do
      ref_user = Factory.insert(:user)
      ref_bot = Factory.insert(:bot, %{user: ref_user})

      ref_user_items =
        for _ <- 1..@num_items do
          Factory.insert(:home_stream_item, %{
            user: shared.user,
            reference_user: ref_user
          })
        end

      ref_bot_items =
        for _ <- 1..@num_items do
          Factory.insert(:home_stream_item, %{
            user: shared.user,
            reference_bot: ref_bot
          })
        end

      Factory.insert(:home_stream_item, %{
        user: shared.user,
        reference_bot: ref_bot,
        class: :ref_update
      })

      user_ids = Enum.map(ref_user_items, & &1.id)
      HomeStream.delete_by_user_ref(ref_user)

      referenced_user_items =
        shared.user.id
        |> HomeStream.get()
        |> Enum.filter(&Enum.member?(user_ids, &1.id))

      bot_ids = Enum.map(ref_bot_items, & &1.id)
      HomeStream.delete_by_bot_ref(ref_bot)

      referenced_bot_items =
        shared.user.id
        |> HomeStream.get()
        |> Enum.filter(&Enum.member?(bot_ids, &1.id))

      {:ok,
       ref_user: ref_user,
       ref_bot: ref_bot,
       referenced_user_items: referenced_user_items,
       ref_user_times: Enum.map(ref_user_items, & &1.updated_at),
       referenced_bot_items: referenced_bot_items,
       ref_bot_times: Enum.map(ref_bot_items, & &1.updated_at)}
    end

    it "should mark all HS entries with the referenced user as deleted" do
      shared.referenced_user_items |> should(have_length(@num_items))

      Enum.each(shared.referenced_user_items, fn i ->
        i.class |> should(eq :deleted)
      end)
    end

    it "should update the updated_at timestamps on all deleted items" do
      shared.referenced_user_items
      |> Enum.map(& &1.updated_at)
      |> Kernel.--(shared.ref_user_times)
      |> should(have_length length(shared.referenced_user_items))
    end

    it "should mark all HS entries with the referenced bot as deleted" do
      shared.referenced_bot_items |> should(have_length(@num_items))

      Enum.each(shared.referenced_bot_items, fn i ->
        i.class |> should(eq :deleted)
      end)
    end

    it "should update the updated_at timestamps on all deleted items" do
      shared.referenced_bot_items
      |> Enum.map(& &1.updated_at)
      |> Kernel.--(shared.ref_bot_times)
      |> should(have_length length(shared.referenced_bot_items))
    end

    context "deletion of items for users who can no longer see the bot" do
      before do
        shared_to_user = Factory.insert(:user)

        for _ <- 1..@num_items do
          Factory.insert(:home_stream_item, %{
            user: shared_to_user,
            reference_bot: shared.ref_bot
          })

          Factory.insert(:home_stream_item, %{
            user: shared.ref_user,
            reference_bot: shared.ref_bot
          })
        end

        Factory.insert(:share, %{
          bot: shared.ref_bot,
          sharer: shared.ref_user,
          user: shared_to_user
        })

        result = HomeStream.delete_by_bot_ref_invisible(shared.ref_bot)

        {:ok, result: result, shared_to_user: shared_to_user}
      end

      it "should return ok" do
        shared.result |> should(eq :ok)
      end

      it "should remove referenced items from unshared-to users" do
        shared.user.id
        |> HomeStream.get()
        |> Enum.filter(&(&1.reference_bot_id == shared.ref_bot.id))
        |> should(eq [])
      end

      it "should not affect the items of users to whom the bot is shared" do
        shared.shared_to_user.id
        |> HomeStream.get()
        |> Enum.filter(&(&1.reference_bot_id == shared.ref_bot.id))
        |> should(have_length(@num_items))
      end

      it "should not affect the items of the bot owner" do
        shared.ref_user.id
        |> HomeStream.get()
        |> Enum.filter(&(&1.reference_bot_id == shared.ref_bot.id))
        |> should(have_length(@num_items))
      end

      it "should entirely remove ref_update items for the deleted object" do
        HomeStreamItem
        |> where(reference_bot_id: ^shared.ref_bot.id)
        |> where(class: ^:ref_update)
        |> Repo.all()
        |> should(eq [])
      end
    end
  end

  describe "get/1" do
    it "should return all items for a user in chronological order" do
      # shared.items is already in chronological order
      shared.user.id
      |> HomeStream.get()
      |> should_match_items(shared.items)
    end

    it "should return an empty list for a non-existant user" do
      ID.new()
      |> HomeStream.get()
      |> should(eq [])
    end

    it "should reutrn an empty list for a user with no items" do
      Factory.insert(:user, %{server: shared.server}).id
      |> HomeStream.get()
      |> should(eq [])
    end
  end

  describe "get/2" do
    it "should exclude deleted items when requested" do
      HomeStream.delete(shared.user.id, hd(shared.items).key)

      shared.user.id
      |> HomeStream.get(false)
      |> should_match_items(tl(shared.items))
    end
  end

  describe "get/3" do
    it "should order by update time" do
      shared.user.id
      |> HomeStream.get(true)
      |> should_match_items(shared.items)
    end
  end

  describe "get_by_key/2" do
    it "should return the user's item with the specified key" do
      item = hd(shared.items)

      shared.user.id
      |> HomeStream.get_by_key(item.key)
      |> should_match_item(item)
    end

    it "should return nil if the key doesn't exit" do
      shared.user.id
      |> HomeStream.get_by_key(Factory.new_jid())
      |> should(be_nil())
    end

    it "should return nil if the user doesn't exit" do
      ID.new()
      |> HomeStream.get_by_key(hd(shared.items).key)
      |> should(be_nil())
    end

    it "should exclude deleted items when requested" do
      HomeStream.delete(shared.user.id, hd(shared.items).key)

      shared.user.id
      |> HomeStream.get_by_key(hd(shared.items).key, false)
      |> should(be_nil())
    end
  end

  describe "get_after_time/2" do
    it """
    should return all items with updated_at later than the given time
    including ref_update items
    """ do
      shared.user.id
      |> HomeStream.get_after_time(shared.middle_time)
      |> should_match_items(shared.after_items ++ [shared.update_item])
    end

    it "should honour the requested limit" do
      shared.user.id
      |> HomeStream.get_after_time(shared.middle_time, 2)
      |> should_match_items(Enum.slice(shared.after_items, 0..1))
    end

    it "should return an empty list for a non-existant user" do
      ID.new()
      |> HomeStream.get_after_time(shared.middle_time)
      |> should(eq [])
    end

    it "should return an empty list for a later time" do
      shared.user.id
      |> HomeStream.get_after_time(Timex.now())
      |> should(eq [])
    end
  end

  describe "get_latest_version/1" do
    it "should return the most recent updated_at value for the user" do
      shared.user.id
      |> HomeStream.get_latest_version()
      |> should(eq shared.version)
    end

    it "should return a valid timestamp for a non-existant user" do
      ID.new()
      |> HomeStream.get_latest_version()
      |> should(be_struct DateTime)
    end
  end

  describe "update_ref_bot/1" do
    before do
      user2 = Factory.insert(:user)
      bot = Factory.insert(:bot, %{user: shared.user})

      Factory.insert_list(2, :home_stream_item, %{
        user: shared.user,
        reference_bot: bot
      })

      Factory.insert(:home_stream_item, %{user: user2, reference_bot: bot})

      HomeStream.update_ref_bot(bot)

      user_items = get_all_items(shared.user.id)
      user2_items = get_all_items(user2.id)

      {:ok,
       bot: bot,
       user2: user2,
       user_items: user_items,
       user2_items: user2_items,
       new_item: List.last(user_items),
       new_item2: List.last(user2_items)}
    end

    it "should add a new update item" do
      # base items plus new items plus explit update plus auto-gen update
      shared.user_items |> should(have_length @num_items + 4)
      shared.user2_items |> should(have_length 2)
    end

    it "should have the new item at the end" do
      shared.new_item.class |> should(eq :ref_update)
      shared.new_item.reference_bot_id |> should(eq shared.bot.id)

      shared.new_item2.class |> should(eq :ref_update)
      shared.new_item2.reference_bot_id |> should(eq shared.bot.id)
    end
  end

  defp should_match_items(items, expected) do
    items |> should(have_length length(expected))

    items
    |> Enum.zip(expected)
    |> Enum.map(&should_match_item/1)
  end

  defp should_match_item({item, expected}),
    do: should_match_item(item, expected)

  defp should_match_item(item, expected) do
    # Skip over the user field since we don't preload it
    item
    |> Map.drop([:user, :reference_bot])
    |> should(eq Map.drop(expected, [:user, :reference_bot]))
  end

  defp get_all_items(user_id) do
    user_id
    |> HomeStream.get_query(
      include_deleted: true,
      include_ref_updates: true
    )
    |> exclude(:preload)
    |> Repo.all()
  end
end
