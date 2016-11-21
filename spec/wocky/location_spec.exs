defmodule Wocky.LocationSpec do
  use ESpec
  alias Wocky.Factory
  alias Wocky.Location
  alias Wocky.User
  alias Wocky.Bot
  alias :wocky_notification_handler, as: Handler

  before do
    user = Factory.build(:user)
    jid = User.to_jid(user, "testing")

    bot_list = Factory.build_list(3, :bot, owner: User.to_jid_string(user))
    bot = hd(bot_list)

    bots = Enum.into(bot_list, %{},
                     fn (%Bot{id: id} = b) -> {id, b} end)

    allow :ejabberd_router |> to(accept :route, fn (_, _, _) -> :ok end)
    allow Handler |> to(accept :notify_bot_event, fn (_, _, _) -> :ok end)
    allow User |> to(accept :get_followed_bots, fn (_) -> Map.keys(bots) end)
    allow User |> to(accept :add_bot_event, fn (_, _, _) -> true end)
    allow User |> to(accept :get_last_bot_event, fn (_, _) -> [] end)
    allow Bot |> to(accept :get, fn (key) -> bots[key] end)

    {:shared, user: User.from_jid(jid), jid: jid, bot: bot}
  end

  describe "user location that is inside a bot perimeter" do
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
        expect Handler
        |> to(accepted :notify_bot_event, [shared.jid, shared.bot.id, :enter])
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
        expect Handler |> to_not(accepted :notify_bot_event)
      end
    end
  end

  describe "user location that is outside a bot perimeter" do
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
        expect Handler
        |> to(accepted :notify_bot_event, [shared.jid, shared.bot.id, :exit])
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
        expect Handler |> to_not(accepted :notify_bot_event)
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
        expect Handler |> to_not(accepted :notify_bot_event)
      end
    end
  end

  describe "user with a bot set to 'follow me'" do
    before do
      allow Bot |> to(accept :set_location, fn (_, _) -> :ok end)
      loc = Factory.build(:location)
      {:shared, loc: {loc.lat, loc.lon, loc.accuracy}}
    end

    context "and an expiry in the future" do
      before do
        expiry = :wocky_db.now_to_timestamp(:os.timestamp()) + 86400
        Factory.insert(:bot,
                       owner: User.to_bare_jid_string(shared.user),
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
