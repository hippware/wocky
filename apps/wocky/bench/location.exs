alias Faker.Util
alias Wocky.Bots
alias Wocky.Location
alias Wocky.Repo.Factory

no_bots = Factory.insert(:user)
bots_10 = Factory.insert(:user)
bots_100 = Factory.insert(:user)
bots_1000 = Factory.insert(:user)

b10 = Factory.insert_list(10, :bot, user: bots_10)
b100 = Factory.insert_list(100, :bot, user: bots_100)
b1000 = Factory.insert_list(1000, :bot, user: bots_1000)

c10 = Util.cycle_start(b10)
c100 = Util.cycle_start(b100)
c1000 = Util.cycle_start(b1000)

mk_loc = fn b -> Factory.build(:location, lat: Bot.lat(b), lon: Bot.lon(b)) end

Benchee.run(
  %{
    no_bots: fn l -> Location.set_user_location(no_bots, l) end,
    bots_10_min: fn l -> Location.set_user_location(bots_10, l) end,
    bots_10_max: fn _ ->
      b = Util.cycle(c10)
      Location.set_user_location(bots_10, mk_loc.(b))
    end,
    bots_100_min: fn l -> Location.set_user_location(bots_100, l) end,
    bots_100_max: fn _ ->
      b = Util.cycle(c100)
      Location.set_user_location(bots_100, mk_loc.(b))
    end,
    bots_1000_min: fn l -> Location.set_user_location(bots_1000, l) end,
    bots_1000_max: fn _ ->
      b = Util.cycle(c1000)
      Location.set_user_location(bots_1000, mk_loc.(b))
    end
  },
  before_each: fn _ -> Factory.build(:location) end
)
