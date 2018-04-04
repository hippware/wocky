defmodule WockyAPI.PublicTest do
  use WockyAPI.ConnCase, async: true

  alias Wocky.Bot.Subscription
  alias Wocky.Repo.Factory

  setup do
    [user, user2] = Factory.insert_list(2, :user)
    bot = Factory.insert(:bot, user: user, public: true)
    bot2 = Factory.insert(:bot, user: user2)
    Subscription.put(user, bot)
    Subscription.put(user2, bot)

    {:ok, user: user, user2: user2, bot: bot, bot2: bot2, conn: build_conn()}
  end

  @query """
  query ($id: String) {
    bot (id: $id) {
      id
      subscribers (first: 10, type: SUBSCRIBER) {
        totalCount
        edges {
          node {
            id
            bots (first: 10, relationship: OWNED) {
              totalCount
              edges {
                node {
                  id
                }
              }
            }
          }
        }
      }
    }
  }
  """
  test "get public bot, subscribers, and their public owned bots",
  %{conn: conn, bot: %{id: bot_id} = bot,
    user: %{id: user_id}, user2: %{id: user2_id}} do
    assert post_conn(conn, @query, %{id: bot.id}, 200) ==
      %{
        "data" => %{
          "bot" => %{
            "id" => bot_id,
            "subscribers" => %{
              "totalCount" => 2,
              "edges" => [
                %{
                  "node" => %{
                    "id" => user_id,
                    "bots" => %{
                      "totalCount" => 1,
                      "edges" => [
                        %{
                          "node" => %{
                            "id" => bot_id
                          }
                        }]
                    }
                  }
                },
                %{
                  "node" => %{
                    "id" => user2_id,
                    "bots" => %{
                      "totalCount" => 0,
                      "edges" => []
                    }
                  }
                }]
            }
          }
        }
      }
    end
end

