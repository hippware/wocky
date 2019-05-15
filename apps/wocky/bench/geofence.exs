alias Wocky.Repo.Factory
alias Wocky.User.GeoFence

no_bots = Factory.insert(:user)
bots_10 = Factory.insert(:user)
bots_100 = Factory.insert(:user)
bots_1000 = Factory.insert(:user)

b10 = Factory.insert_list(10, :bot, user: bots_10)
b100 = Factory.insert_list(100, :bot, user: bots_100)
b1000 = Factory.insert_list(1000, :bot, user: bots_1000)

Benchee.run(
  %{
    no_bots: fn l -> GeoFence.check_for_bot_events(l, no_bots, [], %{}) end,
    bots_10: fn l -> GeoFence.check_for_bot_events(l, bots_10, b10, %{}) end,
    bots_100: fn l -> GeoFence.check_for_bot_events(l, bots_100, b100, %{}) end,
    bots_1000: fn l -> GeoFence.check_for_bot_events(l, bots_1000, b1000, %{}) end
  },
  before_each: fn _ -> Factory.build(:location) end
)
