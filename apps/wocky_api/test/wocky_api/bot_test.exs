defmodule WockyAPI.BotTest do
  use WockyAPI.ConnCase, async: true

  alias Faker.Lorem
  alias Wocky.Account
  alias Wocky.Bot
  alias Wocky.Bot.Subscription
  alias Wocky.GeoUtils
  alias Wocky.Repo.Factory

  setup do
    [user, user2] = Factory.insert_list(2, :user)
    {:ok, {token, _}} = Account.assign_token(user.id, "abc")

    bot = Factory.insert(:bot, user: user)
    bot2 = Factory.insert(:bot, user: user2)
    conn =
      build_conn()
      |> put_req_header("x-auth-user", user.id)
      |> put_req_header("x-auth-token", token)
    {:ok, user: user, user2: user2, bot: bot, bot2: bot2, conn: conn}
  end

  @query """
  mutation ($input: BotSubscribeInput!) {
    botSubscribe (input: $input) {
      result
    }
  }
  """
  test "subscribe", %{conn: conn, user: user, bot2: bot2} do
    assert post_conn(conn, @query, %{input: %{id: bot2.id}}, 200) ==
      %{
        "data" => %{
          "botSubscribe" => %{
            "result" => true
          }
        }
      }
    refute Subscription.get(user, bot2) == nil
  end

  @query """
  mutation ($input: BotUnsubscribeInput!) {
    botUnsubscribe (input: $input) {
      result
    }
  }
  """
  test "unsubscribe", %{conn: conn, user: user, bot2: bot2} do
    Subscription.put(user, bot2)
    assert post_conn(conn, @query, %{input: %{id: bot2.id}}, 200) ==
      %{
        "data" => %{
          "botUnsubscribe" => %{
            "result" => true
          }
        }
      }
    assert Subscription.get(user, bot2) == nil
  end

  @query """
  mutation ($input: BotCreateInput!) {
    botCreate (input: $input) {
      successful
      result {
        id
      }
    }
  }
  """
  test "create bot", %{conn: conn} do
    fields = [:title, :server, :lat, :lon, :radius, :description, :shortname]
    bot = :bot |> Factory.build() |> add_lat_lon() |> Map.take(fields)
    assert %{
      "data" => %{
        "botCreate" => %{
          "successful" => true,
          "result" => %{
            "id" => id
          }
        }
      }
    }
    = post_conn(conn, @query, %{input: %{values: bot}}, 200)

    assert ^bot = id |> Bot.get() |> add_lat_lon() |> Map.take(fields)
  end

  @query """
  mutation ($input: BotUpdateInput!) {
    botUpdate (input: $input) {
      successful
      result {
        id
      }
    }
  }
  """
  test "update bot", %{conn: conn, bot: bot} do
    new_title = Lorem.sentence()
    assert %{
      "data" => %{
        "botUpdate" => %{
          "successful" => true,
          "result" => %{
            "id" => bot.id
          },
        }
      }
    }
    == post_conn(conn, @query,
                 %{input: %{id: bot.id, values: %{title: new_title}}}, 200)

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
  test "get a single bot with subscribers by type",
  %{conn: conn, bot: %{id: id, title: title} = bot, user: user, user2: user2} do
    Bot.subscribe(bot, user2, true)
    assert post_conn(conn, @query, %{id: id, type: "SUBSCRIBER"}, 200) ==
      %{
        "data" => %{
          "bot" => %{
            "id" => id,
            "title" => title,
            "owner" => %{
              "id" => user.id
            },
            "subscribers" => %{
              "totalCount" => 1,
              "edges" => [%{
                "relationships" => ["GUEST", "SUBSCRIBED", "VISIBLE"],
                "node" => %{
                  "id" => user2.id
                }
              }]
            }
          }
        }
      }
  end
  test "get a single bot with subscribers by id",
  %{conn: conn, bot: %{id: id, title: title} = bot, user: user} do
    Bot.subscribe(bot, user)
    assert post_conn(conn, @query, %{id: id, user_id: user.id}, 200) ==
      %{
        "data" => %{
          "bot" => %{
            "id" => id,
            "title" => title,
            "owner" => %{
              "id" => user.id
            },
            "subscribers" => %{
              "totalCount" => 1,
              "edges" => [%{
                "relationships" => ["SUBSCRIBED", "OWNED", "VISIBLE"],
                "node" => %{
                  "id" => user.id
                }
              }]
            }
          }
        }
      }
  end

  defp add_lat_lon(%Bot{location: location} = bot) do
    {lat, lon} = GeoUtils.get_lat_lon(location)
    bot |> Map.put(:lat, lat) |> Map.put(:lon, lon)
  end
end
