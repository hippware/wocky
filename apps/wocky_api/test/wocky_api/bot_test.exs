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
  mutation ($id: String!) {
    subscribeBot (id: $id)
  }
  """
  test "subscribe", %{conn: conn, user: user, bot2: bot2} do
    assert post_conn(conn, @query, %{id: bot2.id}, 200) ==
      %{
        "data" => %{
          "subscribeBot" => true
        }
      }
    refute Subscription.get(user, bot2) == nil
  end

  @query """
  mutation ($id: String!) {
    unsubscribeBot (id: $id)
  }
  """
  test "unsubscribe", %{conn: conn, user: user, bot2: bot2} do
    Subscription.put(user, bot2)
    assert post_conn(conn, @query, %{id: bot2.id}, 200) ==
      %{
        "data" => %{
          "unsubscribeBot" => true
        }
      }
    assert Subscription.get(user, bot2) == nil
  end

  @query """
  mutation ($title: String, $server: String, $lat: Float, $lon: Float,
  $radius: Float, $description: String, $shortname: String, $id: String) {
  insertBot (id: $id, bot: {title: $title, server: $server,
    lat: $lat, lon: $lon, radius: $radius, description: $description,
    shortname: $shortname}) {
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
        "insertBot" => %{
          "successful" => true,
          "result" => %{
            "id" => id
          }
        }
      }
    }
    = post_conn(conn, @query, bot, 200)

    assert ^bot = id |> Bot.get() |> add_lat_lon() |> Map.take(fields)
  end
  test "update bot", %{conn: conn, bot: bot} do
    new_title = Lorem.sentence()
    assert %{
      "data" => %{
        "insertBot" => %{
          "successful" => true,
          "result" => %{
            "id" => bot.id
          },
        }
      }
    }
    == post_conn(conn, @query, %{id: bot.id, title: new_title}, 200)

    assert new_title == Bot.get(bot.id).title
  end

  @query """
  query ($id: String!) {
    bot (id: $id) {
      id
      title
    }
  }
  """
  test "get a single bot", %{conn: conn, bot: %{id: id, title: title}} do
    assert post_conn(conn, @query, %{id: id}, 200) ==
      %{
        "data" => %{
          "bot" => %{
            "id" => id,
            "title" => title
          }
        }
      }
  end

  defp add_lat_lon(%Bot{location: location} = bot) do
    {lat, lon} = GeoUtils.get_lat_lon(location)
    bot |> Map.put(:lat, lat) |> Map.put(:lon, lon)
  end
end
