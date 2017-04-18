defmodule Wocky.User.LocationSpec do
  use ESpec
  use Wocky.JID

  alias Wocky.Bot
  alias Wocky.PushNotifier
  alias Wocky.Repo.Factory
  alias Wocky.Repo.Timestamp
  alias Wocky.User
  alias Wocky.User.Location

  before do
    # owner = Factory.insert(:user)
    # user = Factory.insert(:user)
    # jid = User.to_jid(user, "testing")

    # bot_list = Factory.insert_list(3, :bot, user_id: owner.id)
    # bot = hd(bot_list)

    # bots = Enum.into(bot_list, %{},
    #                  fn (%Bot{id: id} = b) -> {id, b} end)
    # bot_jids = Enum.map(bot_list, &Bot.to_jid(&1))

    # allow :ejabberd_router |> to(accept :route, fn (_, _, _) -> :ok end)
    # allow PushNotifier |> to(accept :push, fn (_, _) -> :ok end)
    # allow User |> to(accept :get_subscribed_bots, fn (_) -> bot_jids end)
    # allow User |> to(accept :add_bot_event, fn (_, _, _) -> true end)
    # allow User |> to(accept :get_last_bot_event, fn (_, _) -> [] end)
    # allow Bot |> to(accept :get, fn (key) -> bots[key] end)

    # {:ok, user: user, jid: jid, bot: bot}
  end

  finally do
    # Repo.delete_all(User)
    # Repo.delete_all(Bot)
  end

  describe "store/5" do

  end

  describe "check_for_bot_events/1" do
    xcontext "with a user location that is inside a bot perimeter" do
      before do
        {:shared, inside_loc: {shared.bot.lat, shared.bot.lon, 10}}
      end

      context "when there are no existing enter events" do
        before do
          :ok = Location.user_location_changed(shared.jid,
                                              shared.inside_loc,
                                              false)
        end

        it "should generate an enter event" do
          expect User
          |> to(accepted :add_bot_event, [shared.user, shared.bot.id, :enter])
        end

        it "should generate a notification" do
          expect :ejabberd_router |> to(accepted :route)
        end

        it "should generate a push notification" do
          expect PushNotifier |> to(accepted :push)
        end
      end

      context "when there is already an existing enter event" do
        before do
          bot_id = shared.bot.id
          allow User
          |> to(accept :get_last_bot_event,
                      fn (_, ^bot_id) -> [%{event: "enter"}]
                          (_, _) -> []
                      end)

          :ok = Location.user_location_changed(shared.jid,
                                              shared.inside_loc,
                                              false)
        end

        it "should not generate an enter event" do
          expect User |> to_not(accepted :add_bot_event)
        end

        it "should not generate a notification" do
          expect :ejabberd_router |> to_not(accepted :route)
        end

        it "should not generate a push notification" do
          expect PushNotifier |> to_not(accepted :push)
        end
      end
    end

    xcontext "with a user location that is outside a bot perimeter" do
      before do
        loc = Factory.build(:location)
        {:shared, outside_loc: {loc.lat, loc.lon, loc.accuracy}}
      end

      context "when there is already an existing enter event" do
        before do
          bot_id = shared.bot.id
          allow User
          |> to(accept :get_last_bot_event,
                      fn (_, ^bot_id) -> [%{event: "enter"}]
                          (_, _) -> []
                      end)

          :ok = Location.user_location_changed(shared.jid,
                                              shared.outside_loc,
                                              false)
        end

        it "should generate an exit event" do
          expect User
          |> to(accepted :add_bot_event, [shared.user, shared.bot.id, :exit])
        end

        it "should generate a notification" do
          expect :ejabberd_router |> to(accepted :route)
        end

        it "should generate a push notification" do
          expect PushNotifier |> to(accepted :push)
        end
      end

      context "when there is already an existing exit event" do
        before do
          bot_id = shared.bot.id
          allow User
          |> to(accept :get_last_bot_event,
                      fn (_, ^bot_id) -> [%{event: "exit"}]
                          (_, _) -> []
                      end)

          :ok = Location.user_location_changed(shared.jid,
                                              shared.outside_loc,
                                              false)
        end

        it "should not generate an exit event" do
          expect User |> to_not(accepted :add_bot_event)
        end

        it "should not generate a notification" do
          expect :ejabberd_router |> to_not(accepted :route)
        end

        it "should not generate a push notification" do
          expect PushNotifier |> to_not(accepted :push)
        end
      end

      context "when there are no events" do
        before do
          :ok = Location.user_location_changed(shared.jid,
                                              shared.outside_loc,
                                              false)
        end

        it "should not generate an exit event" do
          expect User |> to_not(accepted :add_bot_event)
        end

        it "should not generate a notification" do
          expect :ejabberd_router |> to_not(accepted :route)
        end

        it "should not generate a push notification" do
          expect PushNotifier |> to_not(accepted :push)
        end
      end
    end
  end

  describe "update_bot_locations/1" do
    xcontext "with a user that has a bot set to 'follow me'" do
      before do
        allow Bot |> to(accept :set_location, fn (_, _) -> :ok end)
        loc = Factory.build(:location)
        {:shared, loc: {loc.lat, loc.lon, loc.accuracy}}
      end

      context "and an expiry in the future" do
        before do
          expiry = Timestamp.now + 86400
          Factory.insert(:bot,
                        user_id: shared.user.id,
                        follow_me: true,
                        follow_me_expiry: expiry)

          :ok = Location.user_location_changed(shared.jid, shared.loc, false)
        end

        it "should update the bot location" do
          expect Bot |> to(accepted :set_location)
        end
      end

      context "and an expiry in the past" do
        before do
          expiry = :wocky_db.now_to_timestamp(:os.timestamp()) - 86400
          Factory.insert(:bot,
                        owner: User.to_bare_jid_string(shared.user),
                        follow_me: true,
                        follow_me_expiry: expiry)

          :ok = Location.user_location_changed(shared.jid, shared.loc, false)
        end

        it "should not update the bot location" do
          expect Bot |> to_not(accepted :set_location)
        end
      end
    end
  end
end
