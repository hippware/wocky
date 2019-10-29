defmodule WockyAPI.GraphQL.LocationTest do
  # Tests that require location processing

  use WockyAPI.GraphQLCase, async: false

  import WockyAPI.WatcherHelper

  alias Faker.Lorem
  alias Wocky.Friends
  alias Wocky.GeoUtils
  alias Wocky.Location
  alias Wocky.Location.UserLocation
  alias Wocky.POI
  alias Wocky.Relation
  alias Wocky.Repo.Factory

  setup do
    user = Factory.insert(:user)

    {:ok, user: user}
  end

  describe "user location mutations" do
    @query """
    mutation ($input: UserLocationUpdateInput!) {
      userLocationUpdate (input: $input) {
        result {
          watched
          watchers
        }
        successful
      }
    }
    """

    test "set location", %{user: user} do
      Location.inc_watcher_count(user)

      lat = :rand.uniform() * 89.0
      lon = :rand.uniform() * 179.0
      accuracy = :rand.uniform() * 10.0
      device = Factory.device()

      location_input = %{
        "lat" => lat,
        "lon" => lon,
        "accuracy" => accuracy,
        "device" => device,
        "isFetch" => true
      }

      result = run_query(@query, user, %{"input" => location_input})

      refute has_errors(result)

      assert result.data == %{
               "userLocationUpdate" => %{
                 "result" => %{
                   "watched" => true,
                   "watchers" => 1
                 },
                 "successful" => true
               }
             }

      assert %UserLocation{
               lat: ^lat,
               lon: ^lon,
               device: ^device,
               accuracy: ^accuracy
             } = Location.get_current_user_location(user)
    end

    test "invalid location", %{user: user} do
      location_input = %{
        "lat" => :rand.uniform() * 89.0,
        "lon" => :rand.uniform() * 179.0,
        "accuracy" => -1.0,
        "device" => Factory.device()
      }

      result = run_query(@query, user, %{"input" => location_input})

      refute has_errors(result)

      assert result.data == %{
               "userLocationUpdate" => %{
                 "result" => nil,
                 "successful" => false
               }
             }

      refute Location.get_current_user_location(user)
    end

    @query """
    mutation ($input: UserLocationLiveShareInput!) {
      userLocationLiveShare (input: $input) {
        successful
        messages {
          field
          message
        }
        result {
          user { id }
          sharedWith { id }
          expiresAt
        }
      }
    }
    """

    test "start sharing location with current location", %{user: user} do
      user2 = Factory.insert(:user)
      Friends.befriend(user, user2)

      sharer = user.id
      shared_with = user2.id
      expiry = sharing_expiry()
      loc = Factory.build(:location)

      result =
        run_query(@query, user, %{
          "input" => %{
            "sharedWithId" => shared_with,
            "expiresAt" => expiry,
            "location" => %{
              "lat" => loc.lat,
              "lon" => loc.lon,
              "accuracy" => loc.accuracy,
              "device" => loc.device
            }
          }
        })

      refute has_errors(result)

      assert %{
               "userLocationLiveShare" => %{
                 "successful" => true,
                 "result" => %{
                   "user" => %{"id" => ^sharer},
                   "sharedWith" => %{"id" => ^shared_with}
                 }
               }
             } = result.data
    end
  end

  describe "bot mutations with location" do
    setup do
      require_watcher()
      Wocky.Callbacks.Bot.register()

      user = Factory.insert(:user)

      {:ok, user: user}
    end

    @query """
    mutation ($values: BotParams, $user_location: UserLocationUpdateInput) {
      botCreate (input: {values: $values, user_location: $user_location}) {
        successful
        result {
          id
        }
      }
    }
    """
    test "create bot with location", %{user: %{id: user_id} = user} do
      bot_data =
        :bot
        |> Factory.build()
        |> add_bot_lat_lon()
        |> Map.take(bot_create_fields())

      result =
        run_query(@query, user, %{
          "values" => stringify_keys(bot_data),
          "user_location" => %{
            "lat" => bot_data.lat,
            "lon" => bot_data.lon,
            "accuracy" => 1,
            "device" => Lorem.word()
          }
        })

      refute has_errors(result)

      assert %{
               "botCreate" => %{
                 "successful" => true,
                 "result" => %{
                   "id" => id
                 }
               }
             } = result.data

      bot = POI.get(id)
      assert [%{id: ^user_id}] = Relation.get_visitors(bot)
    end

    @query """
    mutation ($id: UUID!, $values: BotParams!,
              $user_location: UserLocationUpdateInput) {
      botUpdate (input: {id: $id, values: $values,
                 user_location: $user_location}) {
        successful
        result {
          id
        }
      }
    }
    """
    test "update bot with location", %{user: %{id: user_id} = user} do
      bot = Factory.insert(:bot, user: user)
      new_title = Lorem.sentence()

      assert [] = Relation.get_visitors(bot)

      result =
        run_query(@query, user, %{
          "id" => bot.id,
          "values" => %{"title" => new_title},
          "user_location" => %{
            "lat" => POI.lat(bot),
            "lon" => POI.lon(bot),
            "accuracy" => 1,
            "device" => Lorem.word()
          }
        })

      refute has_errors(result)

      assert result.data == %{
               "botUpdate" => %{
                 "successful" => true,
                 "result" => %{
                   "id" => bot.id
                 }
               }
             }

      assert new_title == POI.get(bot.id).title
      assert [%{id: ^user_id}] = Relation.get_visitors(bot)
    end
  end

  describe "bot subscriptions with location" do
    @query """
    mutation ($id: UUID!, $guest: Boolean,
              $user_location: UserLocationUpdateInput) {
      botSubscribe (input:
        {id: $id, guest: $guest, user_location: $user_location}) {
        result
        messages  {
          field
          message
        }
      }
    }
    """

    setup ctx do
      user2 = Factory.insert(:user)

      Friends.befriend(ctx.user, user2)
      unsubbed_bot = Factory.insert(:bot, user: user2)

      Factory.insert(:bot_invitation,
        user: user2,
        invitee: ctx.user,
        bot: unsubbed_bot
      )

      {:ok, user2: user2, unsubbed_bot: unsubbed_bot}
    end

    test "subscribe with location inside bot", %{user: user, unsubbed_bot: bot} do
      result =
        run_query(@query, user, %{
          "id" => bot.id,
          "guest" => true,
          "user_location" => %{
            "lat" => POI.lat(bot),
            "lon" => POI.lon(bot),
            "accuracy" => 1,
            "device" => Lorem.word()
          }
        })

      refute has_errors(result)

      assert result.data == %{
               "botSubscribe" => %{"result" => true, "messages" => []}
             }

      assert Relation.visiting?(user, bot)
    end

    test "subscribe with location outside bot", %{user: user, unsubbed_bot: bot} do
      result =
        run_query(@query, user, %{
          "id" => bot.id,
          "guest" => true,
          "user_location" => %{
            "lat" => POI.lat(bot) + 5.0,
            "lon" => POI.lon(bot) + 5.0,
            "accuracy" => 1,
            "device" => Lorem.word()
          }
        })

      refute has_errors(result)

      assert result.data == %{
               "botSubscribe" => %{"result" => true, "messages" => []}
             }

      assert Relation.subscribed?(user, bot)
    end

    test "subscribe with invalid location", %{user: user, unsubbed_bot: bot} do
      result =
        run_query(@query, user, %{
          "id" => bot.id,
          "guest" => true,
          "user_location" => %{
            "lat" => POI.lat(bot),
            "lon" => POI.lon(bot),
            "accuracy" => -1,
            "device" => Lorem.word()
          }
        })

      assert result.data == %{
               "botSubscribe" => %{
                 "result" => nil,
                 "messages" => [
                   %{
                     "field" => "accuracy",
                     "message" => "must be greater than or equal to 0"
                   }
                 ]
               }
             }
    end
  end

  describe "responding to bot invitations" do
    setup %{user: user} do
      bot = Factory.insert(:bot, user: user)
      invitee = Factory.insert(:user)
      Friends.befriend(user, invitee)

      %{id: id} =
        Factory.insert(:bot_invitation,
          user: user,
          bot: bot,
          invitee: invitee
        )

      {:ok, invitee: invitee, bot: bot, id: id}
    end

    @query """
    mutation ($input: BotInvitationRespondInput) {
      botInvitationRespond (input: $input) {
        result
      }
    }
    """

    test "accepting with location", %{id: id} = ctx do
      {lat, lon} = GeoUtils.get_lat_lon(ctx.bot.location)

      result =
        run_query(@query, ctx.invitee, %{
          "input" => %{
            "invitation_id" => to_string(id),
            "accept" => true,
            "user_location" => %{
              "lat" => lat,
              "lon" => lon,
              "accuracy" => 1,
              "device" => Lorem.word()
            }
          }
        })

      refute has_errors(result)

      user_id = ctx.invitee.id

      assert [%{id: ^user_id}] = Relation.get_visitors(ctx.bot)
    end
  end
end
