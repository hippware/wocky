defmodule WockyAPI.GraphQL.BotTest do
  use WockyAPI.GraphQLCase, async: true

  alias Faker.Lorem
  alias Wocky.Bot
  alias Wocky.Bot.Subscription
  alias Wocky.GeoUtils
  alias Wocky.Repo.Factory
  alias Wocky.Repo.ID
  alias Wocky.TROS

  setup do
    [user, user2] = Factory.insert_list(2, :user)

    bot = Factory.insert(:bot, user: user)
    bot2 = Factory.insert(:bot, user: user2)

    {:ok, user: user, user2: user2, bot: bot, bot2: bot2}
  end

  test "subscribe", %{user: user, bot2: bot2} do
    query = """
    mutation ($id: UUID!) {
      botSubscribe (input: {id: $id}) {
        result
      }
    }
    """

    result = run_query(query, user, %{"id" => bot2.id})

    refute has_errors(result)
    assert result.data == %{"botSubscribe" => %{"result" => true}}
    refute Subscription.get(user, bot2) == nil
  end

  test "unsubscribe", %{user: user, bot2: bot2} do
    Subscription.put(user, bot2)

    query = """
    mutation ($id: UUID!) {
      botUnsubscribe (input: {id: $id}) {
        result
      }
    }
    """

    result = run_query(query, user, %{"id" => bot2.id})

    refute has_errors(result)
    assert result.data == %{"botUnsubscribe" => %{"result" => true}}
    assert Subscription.get(user, bot2) == nil
  end

  test "create bot", %{user: user} do
    fields = [:title, :server, :lat, :lon, :radius, :description, :shortname]
    bot = :bot |> Factory.build() |> add_lat_lon() |> Map.take(fields)

    query = """
    mutation ($values: BotParams!) {
      botCreate (input: {values: $values}) {
        successful
        result {
          id
        }
      }
    }
    """

    result = run_query(query, user, %{"values" => stringify_keys(bot)})

    refute has_errors(result)

    assert %{
             "botCreate" => %{
               "successful" => true,
               "result" => %{
                 "id" => id
               }
             }
           } = result.data

    assert ^bot = id |> Bot.get() |> add_lat_lon() |> Map.take(fields)
  end

  test "update bot", %{user: user, bot: bot} do
    new_title = Lorem.sentence()

    query = """
    mutation ($id: UUID!, $values: BotParams!) {
      botUpdate (input: {id: $id, values: $values}) {
        successful
        result {
          id
        }
      }
    }
    """

    result =
      run_query(query, user, %{
        "id" => bot.id,
        "values" => %{"title" => new_title}
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

    assert new_title == Bot.get(bot.id).title
  end

  @query """
  query ($id: String!, $type: SubscriptionType, $user_id: String) {
    bot (id: $id) {
      id
      title
      owner {
        id
      }
      subscribers (first: 1, type: $type, id: $user_id) {
        totalCount
        edges {
          relationships
          node {
            id
          }
        }
      }
    }
  }
  """

  test "get a single bot with subscribers by type", %{
    bot: %{id: id, title: title} = bot,
    user: user,
    user2: user2
  } do
    Bot.subscribe(bot, user2, true)

    result = run_query(@query, user, %{"id" => id, "type" => "SUBSCRIBER"})

    refute has_errors(result)

    assert result.data == %{
             "bot" => %{
               "id" => id,
               "title" => title,
               "owner" => %{
                 "id" => user.id
               },
               "subscribers" => %{
                 "totalCount" => 1,
                 "edges" => [
                   %{
                     "relationships" => ["GUEST", "SUBSCRIBED", "VISIBLE"],
                     "node" => %{
                       "id" => user2.id
                     }
                   }
                 ]
               }
             }
           }
  end

  test "get a single bot with subscribers by id", %{
    bot: %{id: id, title: title} = bot,
    user: user
  } do
    Bot.subscribe(bot, user)

    result = run_query(@query, user, %{"id" => id, "user_id" => user.id})

    refute has_errors(result)

    assert result.data == %{
             "bot" => %{
               "id" => id,
               "title" => title,
               "owner" => %{
                 "id" => user.id
               },
               "subscribers" => %{
                 "totalCount" => 1,
                 "edges" => [
                   %{
                     "relationships" => ["SUBSCRIBED", "OWNED", "VISIBLE"],
                     "node" => %{
                       "id" => user.id
                     }
                   }
                 ]
               }
             }
           }
  end

  test "bot image", %{bot: %{id: id, image: image}, user: user} do
    query = """
    query ($id: UUID) {
      bot (id: $id) {
        image {
          tros_url
          full_url
          thumbnail_url
        }
      }
    }
    """

    result = run_query(query, user, %{"id" => id})

    refute has_errors(result)

    assert %{
             "bot" => %{
               "image" => %{
                 "tros_url" => ^image,
                 "full_url" => "https://" <> _,
                 "thumbnail_url" => "https://" <> _
               }
             }
           } = result.data
  end

  @query """
  query ($id: UUID) {
    bot (id: $id) {
      items (first: 1) {
        edges {
          node {
            stanza
            media {
              tros_url
              full_url
              thumbnail_url
            }
          }
        }
      }
    }
  }
  """

  test "bot item image", %{bot: bot, user: user} do
    tros_url = TROS.make_url("localhost", ID.new())
    stanza = "<message><image>" <> tros_url <> "</image></message>"
    Factory.insert(:item, bot: bot, stanza: stanza, image: true)

    result = run_query(@query, user, %{"id" => bot.id})

    refute has_errors(result)

    assert %{
             "bot" => %{
               "items" => %{
                 "edges" => [
                   %{
                     "node" => %{
                       "stanza" => ^stanza,
                       "media" => %{
                         "tros_url" => ^tros_url,
                         "full_url" => "https://" <> _,
                         "thumbnail_url" => "https://" <> _
                       }
                     }
                   }
                 ]
               }
             }
           } = result.data
  end

  test "bot item no image", %{bot: bot, user: user} do
    %{stanza: stanza} = Factory.insert(:item, bot: bot)

    result = run_query(@query, user, %{"id" => bot.id})

    refute has_errors(result)

    assert %{
             "bot" => %{
               "items" => %{
                 "edges" => [
                   %{
                     "node" => %{
                       "stanza" => ^stanza,
                       "media" => nil
                     }
                   }
                 ]
               }
             }
           } = result.data
  end

  defp add_lat_lon(%Bot{location: location} = bot) do
    {lat, lon} = GeoUtils.get_lat_lon(location)
    bot |> Map.put(:lat, lat) |> Map.put(:lon, lon)
  end

  defp stringify_keys(map) do
    Enum.into(map, %{}, fn {k, v} -> {to_string(k), v} end)
  end
end
