defmodule Wocky.HomeStreamItemSpec do
  use ESpec, async: true

  alias Faker.Lorem
  alias Wocky.HomeStreamItem
  alias Wocky.Repo.Factory
  alias Wocky.Repo.ID

  @num_items 10

  before_all do
    Wocky.Repo.delete_all(Wocky.User)
  end

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
     after_items: Enum.slice(items, (middle_item+1)..@num_items),
     last_item: List.last(items)}
  end

  describe "put/4" do
    context "when there is no existing record for the key" do
      it "should insert a new record" do
        key = Factory.new_jid()
        from_jid = Factory.new_jid()
        stanza = Lorem.paragraph
        put_result = HomeStreamItem.put(shared.user.id, key, from_jid, stanza)
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

        get_result = HomeStreamItem.get_by_key(shared.user.id, shared.last_item.key)
        get_result |> should(be_struct HomeStreamItem)
        get_result.stanza |> should(eq stanza)
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
      shared.user.id
      |> HomeStreamItem.delete(shared.last_item.key)
      |> should(eq :ok)
      get_result = HomeStreamItem.get_by_key(shared.user.id, shared.last_item.key)
      get_result |> should(be_struct HomeStreamItem)
      get_result.deleted |> should(be_true())
    end

    it "should not fail for a non-existant item" do
      shared.user.id
      |> HomeStreamItem.delete(Factory.new_jid)
      |> should(eq :ok)
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
