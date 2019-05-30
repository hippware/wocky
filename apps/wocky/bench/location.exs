alias Faker.Util
alias Wocky.Bot
alias Wocky.Repo.Factory
alias Wocky.User

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
    no_bots: fn l -> User.set_location(no_bots.id, l) end,
    bots_10_min: fn l -> User.set_location(bots_10.id, l) end,
    bots_10_max: fn _ ->
      b = Util.cycle(c10)
      User.set_location(bots_10.id, mk_loc.(b))
    end,
    bots_100_min: fn l -> User.set_location(bots_100.id, l) end,
    bots_100_max: fn _ ->
      b = Util.cycle(c100)
      User.set_location(bots_100.id, mk_loc.(b))
    end,
    bots_1000_min: fn l -> User.set_location(bots_1000.id, l) end,
    bots_1000_max: fn _ ->
      b = Util.cycle(c1000)
      User.set_location(bots_1000.id, mk_loc.(b))
    end
  },
  before_each: fn _ -> Factory.build(:location) end
)
