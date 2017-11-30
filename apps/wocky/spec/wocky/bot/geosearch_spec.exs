defmodule Wocky.Bot.GeosearchSpec do
  use ESpec, async: true
  use ModelHelpers
  use Wocky.JID
  use Wocky.RSMHelper

  alias Faker.Address
  alias Wocky.Bot
  alias Wocky.Bot.Geosearch
  alias Wocky.GeoUtils

  before do
    user = Factory.insert(:user)
    lat = Address.latitude
    lon = Address.longitude

    {:ok,
      user: user,
      lat: lat,
      lon: lon,
    }
  end

  describe "user_distance_query/4" do
    before do
      Factory.insert_list(5, :bot, user: shared.user)

      visible_bots = Geosearch.get_all_with_owner(shared.lat, shared.lon,
                                                  shared.user.id, shared.user.id)

      {:ok,
        bots: visible_bots,
        middle_id: Enum.at(visible_bots, 2).id,
      }
    end

    context "Queried user is owner" do
      before do
        query =
          &Geosearch.user_distance_query(shared.lat, shared.lon,
                                         shared.user.id, shared.user.id, &1)
        {:ok, query: query}
      end

      it "should be checking against all the user's bots" do
        shared.bots |> should(have_length 5)
      end

      it "should work for index lookups" do
        {results, rsm_out} = shared.query.(rsm_in(max: 10, index: 2))
        results |> should(eq Enum.slice(shared.bots, 2..4))
        rsm_out |> should(eq make_rsm(5, 2, 2, 4, shared.bots))
      end

      it "should not fail for out of range indices" do
        {results, rsm_out} = shared.query.(rsm_in(max: 10, index: 6))
        results |> should(eq [])
        rsm_out |> should(eq make_rsm(5))
      end

      it "should work for id lookups" do
        {results, rsm_out} = shared.query.(
          rsm_in(id: shared.middle_id, direction: :aft))
        results |> should(eq Enum.slice(shared.bots, 3..4))
        rsm_out |> should(eq make_rsm(5, 3, 3, 4, shared.bots))
      end

      it "should work in reverse" do
        {results, rsm_out} = shared.query.(
          rsm_in(id: shared.middle_id, direction: :before))
        results |> should(eq Enum.slice(shared.bots, 0..1))
        rsm_out |> should(eq make_rsm(5, 0, 0, 1, shared.bots))

        {results, rsm_out} = shared.query.(
          rsm_in(id: shared.middle_id, direction: :before, reverse: true))
        results |> should(eq Enum.reverse(Enum.slice(shared.bots, 0..1)))
        rsm_out |> should(eq make_rsm(5, 0, 0, 1, shared.bots))
      end

      it "should limit as requested" do
        {results, rsm_out} = shared.query.(
          rsm_in(id: shared.middle_id, direction: :before, max: 1))
        results |> should(eq [Enum.at(shared.bots, 1)])
        rsm_out |> should(eq make_rsm(5, 1, 1, 1, shared.bots))

        {results, rsm_out} = shared.query.(
          rsm_in(id: shared.middle_id, direction: :before,
                 max: 1, reverse: true))
        results |> should(eq [Enum.at(shared.bots, 1)])
        rsm_out |> should(eq make_rsm(5, 1, 1, 1, shared.bots))
      end
    end

    context "user is other than owner" do
      before do
        owner = shared.user
        user = Factory.insert(:user)

        RosterHelper.make_friends(user, owner)

        other_user = Factory.insert(:user)

        _public_bots = Factory.insert_list(5, :bot, public: true, user: owner)
        shared_bots = Factory.insert_list(5, :bot, user: owner)
        Enum.each(shared_bots,
                  &Factory.insert(:share, user: user, sharer: owner, bot: &1))

        visible_bots = Geosearch.get_all_with_owner(shared.lat, shared.lon,
                                                    user.id, owner.id)

        _other_bots = Factory.insert_list(5, :bot, user: other_user)

        query = &Geosearch.user_distance_query(shared.lat, shared.lon,
                                               user.id, owner.id, &1)
        {:ok,
          bots: visible_bots,
          query: query,
          middle_id: Enum.at(visible_bots, 5).id}
      end

      it "should be checking all the visible bots owned by the selected user" do
        shared.bots |> should(have_length 10)
      end

      it "should work for index lookups" do
        {results, rsm_out} = shared.query.(rsm_in(max: 10, index: 2))
        results |> should(eq Enum.slice(shared.bots, 2..9))
        rsm_out |> should(eq make_rsm(10, 2, 2, 9, shared.bots))
      end

      it "should not fail for out of range indices" do
        {results, rsm_out} = shared.query.(rsm_in(max: 10, index: 10))
        results |> should(eq [])
        rsm_out |> should(eq make_rsm(10))
      end

      it "should work for id lookups" do
        {results, rsm_out} = shared.query.(
          rsm_in(id: shared.middle_id, direction: :aft))
        results |> should(eq Enum.slice(shared.bots, 6..9))
        rsm_out |> should(eq make_rsm(10, 6, 6, 9, shared.bots))
      end

      it "should work in reverse" do
        {results, rsm_out} = shared.query.(
          rsm_in(id: shared.middle_id, direction: :before))
        results |> should(eq Enum.slice(shared.bots, 0..4))
        rsm_out |> should(eq make_rsm(10, 0, 0, 4, shared.bots))

        {results, rsm_out} = shared.query.(
          rsm_in(id: shared.middle_id, direction: :before, reverse: true))
        results |> should(eq Enum.reverse(Enum.slice(shared.bots, 0..4)))
        rsm_out |> should(eq make_rsm(10, 0, 0, 4, shared.bots))
      end

      it "should limit as requested" do
        {results, rsm_out} = shared.query.(
          rsm_in(id: shared.middle_id, direction: :before, max: 2))
        results |> should(eq Enum.slice(shared.bots, 3..4))
        rsm_out |> should(eq make_rsm(10, 3, 3, 4, shared.bots))

        {results, rsm_out} = shared.query.(
          rsm_in(id: shared.middle_id, direction: :before,
                 max: 1, reverse: true))
        results |> should(eq [Enum.at(shared.bots, 4)])
        rsm_out |> should(eq make_rsm(10, 4, 4, 4, shared.bots))
      end

    end
  end

  describe "subscribed_distance_query/4" do
    before do
      5
      |> Factory.insert_list(:bot, public: true)
      |> Enum.map(&Factory.insert(:subscription, user: shared.user, bot: &1))

      bots = Geosearch.get_all(shared.lat, shared.lon, shared.user.id)

      _unsubscribed = Factory.insert_list(5, :bot, public: true)

      _owned = Factory.insert(:bot, user: shared.user)

      query = &Geosearch.subscribed_distance_query(shared.lat, shared.lon,
                                                   shared.user.id, &1)

      {:ok,
       query: query,
       bots: bots,
       middle_id: Enum.at(bots, 2).id
      }
    end

    it "should return all the subscribed bots" do
      {results, rsm_out} = shared.query.(rsm_in())
      results |> should(eq shared.bots)
      rsm_out |> should(eq make_rsm(5, 0, 0, 4, shared.bots))
    end

    it "should limit as requested" do
      {results, rsm_out} = shared.query.(
       rsm_in(id: shared.middle_id, direction: :before, max: 2))
      results |> should(eq Enum.slice(shared.bots, 0..1))
      rsm_out |> should(eq make_rsm(5, 0, 0, 1, shared.bots))

      {results, rsm_out} = shared.query.(
       rsm_in(id: shared.middle_id, direction: :before,
        max: 1, reverse: true))
      results |> should(eq [Enum.at(shared.bots, 1)])
      rsm_out |> should(eq make_rsm(5, 1, 1, 1, shared.bots))
    end

  end

  describe "explore_nearby/5" do
    before do
      table = :ets.new(:table, [:public])
      :ets.insert(table, {:acc, []})
      {:ok, table: table}
    end

    context "lots of bots" do
      before do
        user = shared.user
        owner = Factory.insert(:user)

        followee = Factory.insert(:user)
        RosterHelper.follow(user, followee)

        friend = Factory.insert(:user)
        RosterHelper.make_friends(user, friend)

        _own_bots = Factory.insert_list(5, :bot, user: shared.user)

        subscribed_bots = Factory.insert_list(5, :bot, user:
                                              Factory.insert(:user))
        Enum.each(subscribed_bots,
                  &Factory.insert(:subscription, user: user, bot: &1))

        _public_bots = Factory.insert_list(5, :bot, public: true,
                                           user: followee)

        shared_bots = Factory.insert_list(5, :bot, user: friend)
        Enum.each(shared_bots,
                  &Factory.insert(:share, user: user, sharer: owner, bot: &1))

        searchable_bots = Geosearch.get_all(shared.lat, shared.lon,
                                            user.id, false)

        # Bots from unknown users should never show up, regardlesss of being
        # public or shared
        other_bots = Factory.insert_list(5, :bot, public: true,
                                         user: Factory.insert(:user))
        Enum.each(other_bots,
                  &Factory.insert(:share, user: user, bot: &1))

        {:ok,
          bots: searchable_bots
        }
      end

      it "should have all the searchable bots (circle limit)" do
        Geosearch.explore_nearby(GeoUtils.point(shared.lat, shared.lon),
                                 1_000_000_000.0,
                                 shared.user, 100,
                                 &collect_bots(&1, shared.table))
        |> should(eq :ok)
        get_bots(shared.table) |> should(eq shared.bots)
        get_terminator(shared.table) |> should(eq :no_more_results)
      end
    end

    # Use async: false to avoid hitting search timeouts
    context "explore limits", async: false do
      before do
        Repo.delete_all(Bot)
        other_user = Factory.insert(:user)

        Enum.map(1..10,
                 &Factory.insert(:bot, user: shared.user,
                                 location: GeoUtils.point(&1, &1)))

        Factory.insert(:bot, user: other_user,
                       location: GeoUtils.point(0.0, 0.0))

        bots = Geosearch.get_all(0.0, 0.0, shared.user.id)

        {:ok, bots: bots}
      end

      it "should return all bots for a large radius" do
        Geosearch.explore_nearby(GeoUtils.point(0.0, 0.0), 1_000_000_000.0,
                                 shared.user, 100,
                                 &collect_bots(&1, shared.table))
        |> should(eq :ok)
        get_bots(shared.table) |> should(eq shared.bots)
        get_terminator(shared.table) |> should(eq :no_more_results)
      end

      it "should return all for a large rectangle" do
        Geosearch.explore_nearby(GeoUtils.point(0.0, 0.0),
                                 GeoUtils.point(22.0, 22.0),
                                 shared.user, 100,
                                 &collect_bots(&1, shared.table))
        |> should(eq :ok)
        get_bots(shared.table) |> should(eq shared.bots)
        get_terminator(shared.table) |> should(eq :no_more_results)
      end

      it "should return only bots within the specified radius" do
        Geosearch.explore_nearby(GeoUtils.point(0.0, 0.0), 800_000.0,
                                 shared.user, 100,
                                 &collect_bots(&1, shared.table))
        |> should(eq :ok)
        get_bots(shared.table) |> should(eq Enum.take(shared.bots, 5))
        get_terminator(shared.table) |> should(eq :no_more_results)
      end

      it "should return only bots within the specified rectangle" do
        Geosearch.explore_nearby(GeoUtils.point(0.0, 0.0),
                                 GeoUtils.point(11.0, 11.0),
                                 shared.user, 100,
                                 &collect_bots(&1, shared.table))
        |> should(eq :ok)
        get_bots(shared.table) |> should(eq Enum.take(shared.bots, 5))
        get_terminator(shared.table) |> should(eq :no_more_results)
      end

      it "should return only the specified number of bots" do
        Geosearch.explore_nearby(GeoUtils.point(0.0, 0.0), 1_00_000_000.0,
                                 shared.user, 2,
                                 &collect_bots(&1, shared.table))
        |> should(eq :ok)
        get_bots(shared.table) |> should(eq Enum.take(shared.bots, 2))
        get_terminator(shared.table) |> should(eq :result_limit_reached)
      end

      it "should stop after it reaches the query time limit" do
        Application.put_env(:wocky, :max_explore_time, 0)
        Geosearch.explore_nearby(GeoUtils.point(0.0, 0.0), 1_00_000_000.0,
                                 shared.user, 100,
                                 &collect_bots(&1, shared.table))
        |> should(eq :ok)
        Application.delete_env(:wocky, :max_explore_time)

        get_bots(shared.table) |> should(eq Enum.take(shared.bots, 1))
        get_terminator(shared.table) |> should(eq :max_explore_time)
      end

      it "should stop when it has searched the maximum number of bots" do
        Application.put_env(:wocky, :max_explored_bots, 3)
        Geosearch.explore_nearby(GeoUtils.point(0.0, 0.0), 1_00_000_000.0,
                                 shared.user, 100,
                                 &collect_bots(&1, shared.table))
        |> should(eq :ok)
        Application.delete_env(:wocky, :max_explored_bots)

        get_bots(shared.table) |> should(eq Enum.take(shared.bots, 2))
        get_terminator(shared.table) |> should(eq :no_more_results)
      end
    end
  end

  defp make_rsm(count), do: rsm_out(count: count)
  defp make_rsm(count, index, first, last, bots) do
    rsm_out(count: count, index: index,
            first: Enum.at(bots, first).id,
            last: Enum.at(bots, last).id)
  end

  defp collect_bots(bot = %Bot{}, table) do
    [{:acc, acc}] = :ets.lookup(table, :acc)
    :ets.insert(table, {:acc, [bot | acc]})
  end
  defp collect_bots(terminator, table) do
    :ets.insert(table, {:terminator, terminator})
  end

  defp get_bots(table) do
    table
    |> :ets.lookup(:acc)
    |> hd
    |> elem(1)
    |> Enum.reverse
  end

  defp get_terminator(table) do
    case :ets.lookup(table, :terminator) do
      [] -> nil
      [{:terminator, terminator}] -> terminator
    end
  end

end
