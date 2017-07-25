defmodule Wocky.Bot.GeosearchSpec do
  use ESpec, async: true
  use ModelHelpers
  use Wocky.JID
  use Wocky.RSMHelper

  alias Faker.Address
  alias Wocky.Bot.Geosearch

  describe "distance_query/4" do
    before do
      user = Factory.insert(:user)
      Factory.insert_list(5, :bot, user: user)

      lat = Address.latitude
      lon = Address.longitude

      bots = Geosearch.get_all(lat, lon, user.id, user.id)

      {:ok,
        bots: bots,
        middle_id: Enum.at(bots, 2).id,
        user: user,
        lat: lat,
        lon: lon,
      }
    end

    context "Queried user is owner" do
      before do
        query = fn(rsm) ->
          Geosearch.distance_query(shared.lat, shared.lon,
                                   shared.user.id, shared.user.id,
                                   rsm)
        end
        {:ok, query: query}
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

        searchable_bots = Geosearch.get_all(shared.lat, shared.lon, user.id, owner.id)

        _other_bots = Factory.insert_list(5, :bot, user: other_user)

        query = fn(rsm) ->
          Geosearch.distance_query(shared.lat, shared.lon,
                                   user.id, owner.id, rsm)
        end
        {:ok, query: query}

        {:ok,
          bots: searchable_bots,
          query: query,
          middle_id: Enum.at(searchable_bots, 5).id}
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

    context "owner is not specified (\"explore nearby\")", async: false do
      before do
        user = shared.user
        owner = Factory.insert(:user)

        followee = Factory.insert(:user)
        RosterHelper.follow(user, followee)

        friend = Factory.insert(:user)
        RosterHelper.make_friends(user, friend)

        _own_bots = shared.bots

        subscribed_bots = Factory.insert_list(5, :bot, user: Factory.insert(:user))
        Enum.each(subscribed_bots,
                  &Factory.insert(:subscription, user: user, bot: &1))

        _public_bots = Factory.insert_list(5, :bot, public: true, user: followee)

        shared_bots = Factory.insert_list(5, :bot, user: friend)
        Enum.each(shared_bots,
                  &Factory.insert(:share, user: user, sharer: owner, bot: &1))

        searchable_bots = Geosearch.get_all(shared.lat, shared.lon, user.id, :undefined)

        # Bots from unknown users should never show up, regardlesss of being
        # public or shared
        other_bots = Factory.insert_list(5, :bot, public: true, user: Factory.insert(:user))
        Enum.each(other_bots,
                  &Factory.insert(:share, user: user, bot: &1))

        query = fn(rsm) ->
          Geosearch.distance_query(shared.lat, shared.lon,
                                   user.id, :undefined, rsm)
        end

        {:ok,
          bots: searchable_bots,
          query: query,
          middle_id: Enum.at(searchable_bots, 10).id}
      end

      it "should work for index lookups" do
        {results, rsm_out} = shared.query.(rsm_in(max: 10, index: 2))
        results |> should(eq Enum.slice(shared.bots, 2..11))
        rsm_out |> should(eq make_rsm(20, 2, 2, 11, shared.bots))
      end

      it "should not fail for out of range indices" do
        {results, rsm_out} = shared.query.(rsm_in(max: 10, index: 20))
        results |> should(eq [])
        rsm_out |> should(eq make_rsm(20))
      end

      it "should work for id lookups" do
        {results, rsm_out} = shared.query.(
          rsm_in(id: shared.middle_id, direction: :aft))
        results |> should(eq Enum.slice(shared.bots, 11..19))
        rsm_out |> should(eq make_rsm(20, 11, 11, 19, shared.bots))
      end

      it "should work in reverse" do
        {results, rsm_out} = shared.query.(
          rsm_in(id: shared.middle_id, direction: :before))
        results |> should(eq Enum.slice(shared.bots, 0..9))
        rsm_out |> should(eq make_rsm(20, 0, 0, 9, shared.bots))

        {results, rsm_out} = shared.query.(
          rsm_in(id: shared.middle_id, direction: :before, reverse: true))
        results |> should(eq Enum.reverse(Enum.slice(shared.bots, 0..9)))
        rsm_out |> should(eq make_rsm(20, 0, 0, 9, shared.bots))
      end

      it "should limit as requested" do
        {results, rsm_out} = shared.query.(
          rsm_in(id: shared.middle_id, direction: :before, max: 2))
        results |> should(eq Enum.slice(shared.bots, 8..9))
        rsm_out |> should(eq make_rsm(20, 8, 8, 9, shared.bots))

        {results, rsm_out} = shared.query.(
          rsm_in(id: shared.middle_id, direction: :before,
                 max: 1, reverse: true))
        results |> should(eq [Enum.at(shared.bots, 9)])
        rsm_out |> should(eq make_rsm(20, 9, 9, 9, shared.bots))
      end
    end
  end

  defp make_rsm(count), do: rsm_out(count: count)
  defp make_rsm(count, index, first, last, bots) do
    rsm_out(count: count, index: index,
            first: Enum.at(bots, first).id,
            last: Enum.at(bots, last).id)
  end

end
