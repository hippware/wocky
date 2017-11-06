defmodule Wocky.HomeStreamItemSpec do
  use ESpec, async: true
  use ModelHelpers

  alias Faker.Lorem
  alias Timex.Duration
  alias Wocky.HomeStreamItem
  alias Wocky.Repo.Factory
  alias Wocky.Repo.ID

  @num_items 10

  @differeing_prepop_fields [:user, :user_id, :id]

  before do
    user = Factory.insert(:user, %{server: shared.server})

    items = for _ <- 1..@num_items do
      Factory.insert(:home_stream_item, %{user: user})
    end

    middle_item = div(@num_items, 2) - 1

    {:ok,
     user: user,
     items: items,
     middle_time: Enum.at(items, middle_item).updated_at,
     after_items: Enum.slice(items, (middle_item + 1)..@num_items),
     last_item: List.last(items)}
  end

  describe "put/4" do
    context "when there is no existing record for the key" do
      it "should insert a new record" do
        key = Factory.new_jid
        from_jid = Factory.new_jid
        stanza = Lorem.paragraph
        put_result = HomeStreamItem.put(shared.user.id,
                                        key,
                                        from_jid,
                                        stanza)
        put_result |> should(be_ok_result())
        put_result |> Kernel.elem(1) |> should(be_struct HomeStreamItem)

        get_result = HomeStreamItem.get_by_key(shared.user.id, key)
        get_result |> should(be_struct HomeStreamItem)
        get_result.user_id |> should(eq shared.user.id)
        get_result.key |> should(eq key)
        get_result.from_jid |> should(eq from_jid)
        get_result.stanza |> should(eq stanza)
        get_result.deleted |> should(be_false())
      end

      it "should work even when :set_ordering is false" do
        HomeStreamItem.put(shared.user.id,
                           Factory.new_jid,
                           Factory.new_jid,
                           Lorem.paragraph,
                           set_ordering: false)
        |> should(be_ok_result())
      end
    end

    context "when there is an existing record for the key" do
      it "should overwrite the record" do
        stanza = Lorem.paragraph
        put_result = HomeStreamItem.put(shared.user.id,
                                        shared.last_item.key,
                                        shared.last_item.from_jid,
                                        stanza)
        put_result |> should(be_ok_result())
        put_result |> Kernel.elem(1) |> should(be_struct HomeStreamItem)

        get_result = HomeStreamItem.get_by_key(shared.user.id,
                                               shared.last_item.key)
        get_result |> should(be_struct HomeStreamItem)
        get_result.stanza |> should(eq stanza)
        get_result.ordering |> should(be_later_than(shared.last_item.ordering))
      end

      it "should not update the ordering when set_ordering is false" do
        stanza = Lorem.paragraph
        put_result = HomeStreamItem.put(shared.user.id,
                                        shared.last_item.key,
                                        shared.last_item.from_jid,
                                        stanza,
                                        set_ordering: false)
        put_result |> should(be_ok_result())

        get_result = HomeStreamItem.get_by_key(shared.user.id,
                                               shared.last_item.key)
        get_result.ordering |> should(eq(shared.last_item.ordering))
      end
    end

    context "with referenced user or bot" do
      it "should store the reference" do
        ref_user = Factory.insert(:user)
        ref_bot = Factory.insert(:bot)
        put_result = HomeStreamItem.put(shared.user.id,
                                        shared.last_item.key,
                                        shared.last_item.from_jid,
                                        Lorem.paragraph,
                                        ref_user_id: ref_user.id,
                                        ref_bot_id: ref_bot.id)

        put_result |> Kernel.elem(1) |> should(be_struct HomeStreamItem)

        get_result = HomeStreamItem.get_by_key(shared.user.id,
                                               shared.last_item.key)
        get_result |> should(be_struct HomeStreamItem)
        get_result.reference_user_id |> should(eq ref_user.id)
        get_result.reference_bot_id |> should(eq ref_bot.id)
      end

      it "should fail on invalid references" do
        HomeStreamItem.put(shared.user.id,
                           shared.last_item.key,
                           shared.last_item.from_jid,
                           Lorem.paragraph,
                           ref_user_id: ID.new)
                           |> should(be_error_result())

        HomeStreamItem.put(shared.user.id,
                           shared.last_item.key,
                           shared.last_item.from_jid,
                           Lorem.paragraph,
                           ref_bot_id: ID.new)
                           |> should(be_error_result())
      end
    end

    context "when the user is invalid" do
      it "should cause an exception" do
        fn ->
          HomeStreamItem.put(ID.new, Lorem.word,
                             Factory.make_jid, Lorem.paragraph)
        end
        |> should(raise_exception())
      end
    end
  end

  describe "delete/2" do
    it "should flag an existing item as deleted" do
      result = HomeStreamItem.delete(shared.user.id, shared.last_item.key)
      result |> should(be_ok_result())
      result_val = Kernel.elem(result, 1)
      result_val |> should(be_struct HomeStreamItem)
      result_val.deleted |> should(be_true())
    end

    it "should not fail for a non-existant item" do
      shared.user.id
      |> HomeStreamItem.delete(Factory.new_jid)
      |> should(eq {:ok, nil})
    end
  end

  describe "deletion by reference" do
    before do
      ref_user = Factory.insert(:user)
      ref_bot = Factory.insert(:bot, %{user: ref_user})

      ref_user_items = for _ <- 1..@num_items do
        Factory.insert(:home_stream_item,
                       %{user: shared.user, reference_user: ref_user})
      end

      ref_bot_items = for _ <- 1..@num_items do
        Factory.insert(:home_stream_item,
                       %{user: shared.user, reference_bot: ref_bot})
      end

      user_ids = Enum.map(ref_user_items, &(&1.id))
      HomeStreamItem.delete_by_user_ref(ref_user)
      referenced_user_items =
        shared.user.id
        |> HomeStreamItem.get
        |> Enum.filter(&Enum.member?(user_ids, &1.id))

      bot_ids = Enum.map(ref_bot_items, &(&1.id))
      HomeStreamItem.delete_by_bot_ref(ref_bot)
      referenced_bot_items =
        shared.user.id
        |> HomeStreamItem.get
        |> Enum.filter(&Enum.member?(bot_ids, &1.id))

      {:ok,
        ref_user: ref_user,
        ref_bot: ref_bot,
        referenced_user_items: referenced_user_items,
        ref_user_times: Enum.map(ref_user_items, &(&1.updated_at)),
        referenced_bot_items: referenced_bot_items,
        ref_bot_times: Enum.map(ref_bot_items, &(&1.updated_at))
      }
    end

    it "should mark all HS entries with the referenced user as deleted" do
      shared.referenced_user_items |> should(have_length(@num_items))
      Enum.each(shared.referenced_user_items,
                fn(i) -> i.deleted |> should(be_true()) end)
    end

    it "should update the updated_at timestamps on all deleted items" do
      shared.referenced_user_items
      |> Enum.map(&(&1.updated_at))
      |> Kernel.--(shared.ref_user_times)
      |> should(have_length length(shared.referenced_user_items))
    end

    it "should mark all HS entries with the referenced bot as deleted" do
      shared.referenced_bot_items |> should(have_length(@num_items))
      Enum.each(shared.referenced_bot_items,
                fn(i) -> i.deleted |> should(be_true()) end)
    end

    it "should update the updated_at timestamps on all deleted items" do
      shared.referenced_bot_items
      |> Enum.map(&(&1.updated_at))
      |> Kernel.--(shared.ref_bot_times)
      |> should(have_length length(shared.referenced_bot_items))
    end

    context "deletion of items for users who can no longer see the bot" do
      before do
        shared_to_user = Factory.insert(:user)
        for _ <- 1..@num_items do
          Factory.insert(:home_stream_item,
                         %{user: shared_to_user,
                           reference_bot: shared.ref_bot})
          Factory.insert(:home_stream_item,
                         %{user: shared.ref_user,
                           reference_bot: shared.ref_bot})
        end
        Factory.insert(:share, %{bot: shared.ref_bot,
                                 sharer: shared.ref_user,
                                 user: shared_to_user})

        result = HomeStreamItem.delete_by_bot_ref_invisible(shared.ref_bot)

        {:ok, result: result, shared_to_user: shared_to_user}
      end

      it "should return ok" do
        shared.result |> should(eq :ok)
      end

      it "should remove referenced items from unshared-to users" do
        shared.user.id
        |> HomeStreamItem.get
        |> Enum.filter(&(&1.reference_bot_id == shared.ref_bot.id))
        |> should(eq [])
      end

      it "should not affect the items of users to whom the bot is shared" do
        shared.shared_to_user.id
        |> HomeStreamItem.get
        |> Enum.filter(&(&1.reference_bot_id == shared.ref_bot.id))
        |> should(have_length(@num_items))
      end

      it "should not affect the items of the bot owner" do
        shared.ref_user.id
        |> HomeStreamItem.get
        |> Enum.filter(&(&1.reference_bot_id == shared.ref_bot.id))
        |> should(have_length(@num_items))
      end
    end
  end


  describe "get/1" do
    it "should return all items for a user in chronological order" do
      # shared.items is already in chronological order
      shared.user.id
      |> HomeStreamItem.get
      |> should_match_items(shared.items)
    end

    it "should return an empty list for a non-existant user" do
      ID.new
      |> HomeStreamItem.get
      |> should(eq [])
    end

    it "should reutrn an empty list for a user with no items" do
      Factory.insert(:user, %{server: shared.server}).id
      |> HomeStreamItem.get
      |> should(eq [])
    end
  end

  describe "get/2" do
    it "should exclude deleted items when requested" do
      HomeStreamItem.delete(shared.user.id, hd(shared.items).key)

      shared.user.id
      |> HomeStreamItem.get(true)
      |> should_match_items(tl(shared.items))
    end
  end

  describe "get/3" do
    before do
      time = Timex.shift(DateTime.utc_now(), days: -1)
      item = Factory.insert(:home_stream_item, %{user: shared.user,
                                                 ordering: time})
      {:ok, item: item}
    end

    it "should order by update time if specified" do
      shared.user.id
      |> HomeStreamItem.get(false, true)
      |> List.last
      |> should_match_item(shared.item)
    end

    it "should order by ordering otherwise" do
      shared.user.id
      |> HomeStreamItem.get(false)
      |> hd
      |> should_match_item(shared.item)
    end
  end

  describe "get_by_key/2" do
    it "should return the user's item with the specified key" do
      item = hd(shared.items)
      shared.user.id
      |> HomeStreamItem.get_by_key(item.key)
      |> should_match_item(item)
    end

    it "should return nil if the key doesn't exit" do
      shared.user.id
      |> HomeStreamItem.get_by_key(Factory.new_jid)
      |> should(be_nil())
    end

    it "should return nil if the user doesn't exit" do
      ID.new
      |> HomeStreamItem.get_by_key(hd(shared.items).key)
      |> should(be_nil())
    end

    it "should exclude deleted items when requested" do
      HomeStreamItem.delete(shared.user.id, hd(shared.items).key)

      shared.user.id
      |> HomeStreamItem.get_by_key(hd(shared.items).key, true)
      |> should(be_nil())
    end
  end

  describe "get_after_time/2" do
    it "should return all items with updated_at later than the given time" do
      shared.user.id
      |> HomeStreamItem.get_after_time(shared.middle_time)
      |> should_match_items(shared.after_items)
    end

    it "should return an empty list for a non-existant user" do
      ID.new
      |> HomeStreamItem.get_after_time(shared.middle_time)
      |> should(eq [])
    end

    it "should return an empty list for a later time" do
      shared.user.id
      |> HomeStreamItem.get_after_time(Timex.now)
      |> should(eq [])
    end
  end

  describe "get_latest_time/1" do
    it "should return the most recent updated_at value for the user" do
      shared.user.id
      |> HomeStreamItem.get_latest_time
      |> should(eq shared.last_item.updated_at)
    end

    it "should return a valid timestamp for a non-existant user" do
      ID.new
      |> HomeStreamItem.get_latest_time
      |> should(be_struct DateTime)
    end
  end

  describe "prepopulate_from/3" do
    before do
      source_user = Factory.insert(:user)
      now = DateTime.utc_now

      items = for i <- 1..@num_items do
        time = Timex.shift(now, seconds: -(i * 5))
        Factory.insert(:home_stream_item,
                       %{user: source_user,
                         created_at: time,
                         updated_at: time,
                         ordering: time})
      end

      target_user = Factory.insert(:user)

      {:ok,
        items: Enum.map(Enum.reverse(items),
                        &Map.drop(&1, @differeing_prepop_fields)),
        source_user: source_user,
        target_user: target_user}
    end

    it "should copy items for the specified time period" do
      HomeStreamItem.prepopulate_from(shared.target_user.id,
                                      shared.source_user.id,
                                      Duration.from_days(10))
      |> should(eq :ok)

      shared.target_user.id
      |> HomeStreamItem.get
      |> Enum.map(&Map.drop(&1, @differeing_prepop_fields))
      |> should(eq shared.items)
    end

    it "should not copy items if a zero period is given" do
      HomeStreamItem.prepopulate_from(shared.target_user.id,
                                      shared.source_user.id,
                                      Duration.from_seconds(0))
      |> should(eq :ok)

      shared.target_user.id
      |> HomeStreamItem.get
      |> should(eq [])
    end

    it "should copy only items in the time period" do
      pivot = Enum.at(shared.items, 5)
      period = Timex.diff(DateTime.utc_now(), pivot.created_at(), :duration)

      HomeStreamItem.prepopulate_from(shared.target_user.id,
                                      shared.source_user.id,
                                      period)
      |> should(eq :ok)

      shared.target_user.id
      |> HomeStreamItem.get
      |> Enum.map(&Map.drop(&1, @differeing_prepop_fields))
      |> should(eq Enum.slice(shared.items, 6..@num_items - 1))
    end
  end

  defp should_match_items(items, expected) do
    items |> should(have_length length(expected))
    items
    |> Enum.zip(expected)
    |> Enum.map(&should_match_item/1)
  end

  defp should_match_item({item, expected}), do: should_match_item(item, expected)
  defp should_match_item(item, expected) do
    # Skip over the user field since we don't preload it
    item
    |> Map.drop([:user])
    |> should(eq Map.drop(expected, [:user]))
  end

end
