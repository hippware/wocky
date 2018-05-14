defmodule WockyAPI.GraphQL.BotTest do
  use WockyAPI.GraphQLCase, async: true

  alias Faker.Lorem
  alias Wocky.Bot
  alias Wocky.GeoUtils
  alias Wocky.Repo.Factory
  alias Wocky.Repo.ID
  alias Wocky.TROS

  setup do
    [user, user2] = Factory.insert_list(2, :user)

    bot = Factory.insert(:bot, user: user)
    bot2 = Factory.insert(:bot, user: user2, public: true)

    {:ok, user: user, user2: user2, bot: bot, bot2: bot2}
  end

  describe "basic bot queries" do
    test "get a single bot", %{user: user, bot: bot} do
      query = """
      query ($id: UUID!) {
        bot (id: $id) {
          id
        }
      }
      """

      result = run_query(query, user, %{"id" => bot.id})

      refute has_errors(result)

      assert result.data == %{
               "bot" => %{
                 "id" => bot.id
               }
             }
    end

    @query """
    query ($id: UUID, $relationship: UserBotRelationship) {
      currentUser {
        bots (first: 1, id: $id, relationship: $relationship) {
          totalCount
          edges {
            node {
              id
            }
          }
        }
      }
    }
    """

    test "get owned bots by relationship", %{user: user, bot: bot} do
      result = run_query(@query, user, %{"relationship" => "OWNED"})

      refute has_errors(result)

      assert result.data == %{
               "currentUser" => %{
                 "bots" => %{
                   "totalCount" => 1,
                   "edges" => [
                     %{
                       "node" => %{
                         "id" => bot.id
                       }
                     }
                   ]
                 }
               }
             }
    end

    test "get owned bots by id", %{user: user, bot: bot} do
      result = run_query(@query, user, %{"id" => bot.id})

      refute has_errors(result)

      assert result.data == %{
               "currentUser" => %{
                 "bots" => %{
                   "totalCount" => 1,
                   "edges" => [
                     %{
                       "node" => %{
                         "id" => bot.id
                       }
                     }
                   ]
                 }
               }
             }
    end

    test "get bots with both id and relationship", %{user: user, bot: bot} do
      result =
        run_query(@query, user, %{
          "relationship" => "OWNED",
          "id" => bot.id
        })

      assert error_count(result) == 1
      assert error_msg(result) =~ "Only one of 'id' or 'relationship'"
      assert result.data == %{"currentUser" => %{"bots" => nil}}
    end

    test "get bots with neither id or relationship", %{user: user} do
      result = run_query(@query, user)

      assert error_count(result) == 1
      assert error_msg(result) =~ "'id' or 'relationship' must be specified"
      assert result.data == %{"currentUser" => %{"bots" => nil}}
    end

    @query """
    query ($id: UUID!) {
      user (id: $id) {
        bots (first: 1, relationship: OWNED) {
          totalCount
          edges {
            node {
              id
            }
          }
        }
      }
    }
    """

    test "get bots owned by another user", %{
      user: user,
      user2: user2,
      bot2: bot2
    } do
      result = run_query(@query, user, %{"id" => user2.id})

      refute has_errors(result)

      assert result.data == %{
               "user" => %{
                 "bots" => %{
                   "totalCount" => 1,
                   "edges" => [
                     %{
                       "node" => %{
                         "id" => bot2.id
                       }
                     }
                   ]
                 }
               }
             }
    end

    test "get bots anonymously", %{user2: user2, bot2: bot2} do
      result = run_query(@query, nil, %{"id" => user2.id})

      refute has_errors(result)

      assert result.data == %{
               "user" => %{
                 "bots" => %{
                   "totalCount" => 1,
                   "edges" => [
                     %{
                       "node" => %{
                         "id" => bot2.id
                       }
                     }
                   ]
                 }
               }
             }
    end

    @query """
    query ($last: Int!, $before: String, $relationship: UserBotRelationship!) {
      currentUser {
        bots (last: $last, before: $before, relationship: $relationship) {
          totalCount
          edges {
            cursor
            node {
              id
            }
          }
          pageInfo {
            hasPreviousPage
            hasNextPage
          }
        }
      }
    }
    """

    test "get last items in a list", %{user: user, bot: bot} do
      [b7_id, b8_id, b9_id, b10_id] =
        [bot | Factory.insert_list(10, :bot, user: user)]
        |> Enum.reverse()
        |> Enum.slice(-4..-1)
        |> Enum.map(& &1.id)

      result =
        run_query(@query, user, %{"last" => 3, "relationship" => "OWNED"})

      refute has_errors(result)

      assert %{
               "currentUser" => %{
                 "bots" => %{
                   "totalCount" => 11,
                   "edges" => [
                     %{
                       "cursor" => c8,
                       "node" => %{
                         "id" => ^b8_id
                       }
                     },
                     %{
                       "cursor" => c9,
                       "node" => %{
                         "id" => ^b9_id
                       }
                     },
                     %{
                       "cursor" => _c10,
                       "node" => %{
                         "id" => ^b10_id
                       }
                     }
                   ],
                   "pageInfo" => %{
                     "hasNextPage" => false,
                     "hasPreviousPage" => true
                   }
                 }
               }
             } = result.data

      result =
        run_query(@query, user, %{
          "last" => 2,
          "before" => c9,
          "relationship" => "OWNED"
        })

      refute has_errors(result)

      assert %{
               "currentUser" => %{
                 "bots" => %{
                   "totalCount" => 11,
                   "edges" => [
                     %{
                       "cursor" => _c7,
                       "node" => %{
                         "id" => ^b7_id
                       }
                     },
                     %{
                       "cursor" => ^c8,
                       "node" => %{
                         "id" => ^b8_id
                       }
                     }
                   ],
                   "pageInfo" => %{
                     "hasNextPage" => true,
                     "hasPreviousPage" => true
                   }
                 }
               }
             } = result.data
    end
  end

  describe "active bots" do
    setup %{user: user, bot: bot, user2: user2, bot2: bot2} do
      Bot.subscribe(bot, user, true)
      Bot.subscribe(bot2, user, true)
      Bot.visit(bot, user, false)

      Bot.subscribe(bot2, user2, true)
      Bot.visit(bot2, user2, false)

      for b <- Factory.insert_list(3, :bot, public: true) do
        Bot.subscribe(b, user, true)
      end

      :ok
    end

    @query """
    {
      currentUser {
        activeBots(first: 5) {
          edges {
            node {
              id
              subscribers(first: 5, type: VISITOR) {
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

    test "get active bots", %{user: user, bot: bot, user2: user2, bot2: bot2} do
      result = run_query(@query, user)

      refute has_errors(result)

      assert result.data == %{
               "currentUser" => %{
                 "activeBots" => %{
                   "edges" => [
                     %{
                       "node" => %{
                         "id" => bot2.id,
                         "subscribers" => %{
                           "edges" => [
                             %{"node" => %{"id" => user2.id}}
                           ]
                         }
                       }
                     },
                     %{
                       "node" => %{
                         "id" => bot.id,
                         "subscribers" => %{
                           "edges" => [
                             %{"node" => %{"id" => user.id}}
                           ]
                         }
                       }
                     }
                   ]
                 }
               }
             }
    end
  end

  describe "bot mutations" do
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
  end

  describe "bot subscriptions" do
    @query """
    mutation ($id: UUID!) {
      botSubscribe (input: {id: $id}) {
        result
      }
    }
    """

    test "subscribe", %{user: user, bot2: bot2} do
      result = run_query(@query, user, %{"id" => bot2.id})

      refute has_errors(result)
      assert result.data == %{"botSubscribe" => %{"result" => true}}
      assert Bot.subscription(bot2, user) == :subscribed
    end

    test "subscribe to a non-existent bot", %{user: user} do
      result = run_query(@query, user, %{"id" => ID.new()})

      assert error_count(result) == 1
      assert error_msg(result) =~ "Bot not found"
      assert result.data == %{"botSubscribe" => nil}
    end

    @query """
    mutation ($id: UUID!) {
      botUnsubscribe (input: {id: $id}) {
        result
      }
    }
    """

    test "unsubscribe", %{user: user, bot2: bot2} do
      Bot.subscribe(bot2, user)

      result = run_query(@query, user, %{"id" => bot2.id})

      refute has_errors(result)
      assert result.data == %{"botUnsubscribe" => %{"result" => true}}
      assert Bot.subscription(bot2, user) == nil
    end

    test "unsubscribe from a non-existent bot", %{user: user} do
      result = run_query(@query, user, %{"id" => ID.new()})

      assert error_count(result) == 1
      assert error_msg(result) =~ "Bot not found"
      assert result.data == %{"botUnsubscribe" => nil}
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

    test "get bot subscribers", %{bot: bot, user: user, user2: user2} do
      Bot.subscribe(bot, user2)

      result =
        run_query(@query, user, %{
          "id" => bot.id,
          "type" => "SUBSCRIBER"
        })

      refute has_errors(result)

      assert result.data == %{
               "bot" => %{
                 "id" => bot.id,
                 "title" => bot.title,
                 "owner" => %{
                   "id" => user.id
                 },
                 "subscribers" => %{
                   "totalCount" => 1,
                   "edges" => [
                     %{
                       "relationships" => ["SUBSCRIBED", "VISIBLE"],
                       "node" => %{
                         "id" => user2.id
                       }
                     }
                   ]
                 }
               }
             }
    end

    test "get bot guests", %{bot: bot, user: user, user2: user2} do
      Bot.subscribe(bot, user2, true)

      result = run_query(@query, user, %{"id" => bot.id, "type" => "GUEST"})

      refute has_errors(result)

      assert result.data == %{
               "bot" => %{
                 "id" => bot.id,
                 "title" => bot.title,
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

    test "get bot visitors", %{bot: bot, user: user, user2: user2} do
      Bot.subscribe(bot, user2, true)
      Bot.visit(bot, user2)

      result = run_query(@query, user, %{"id" => bot.id, "type" => "VISITOR"})

      refute has_errors(result)

      assert result.data == %{
               "bot" => %{
                 "id" => bot.id,
                 "title" => bot.title,
                 "owner" => %{
                   "id" => user.id
                 },
                 "subscribers" => %{
                   "totalCount" => 1,
                   "edges" => [
                     %{
                       "relationships" => [
                         "VISITOR",
                         "GUEST",
                         "SUBSCRIBED",
                         "VISIBLE"
                       ],
                       "node" => %{
                         "id" => user2.id
                       }
                     }
                   ]
                 }
               }
             }
    end

    test "get bot subscribers by id", %{bot: bot, user: user} do
      Bot.subscribe(bot, user)

      result = run_query(@query, user, %{"id" => bot.id, "user_id" => user.id})

      refute has_errors(result)

      assert result.data == %{
               "bot" => %{
                 "id" => bot.id,
                 "title" => bot.title,
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

    test "get bot subscribers by both id and type", %{user: user, bot: bot} do
      result =
        run_query(@query, user, %{
          "id" => bot.id,
          "user_id" => user.id,
          "type" => "GUEST"
        })

      assert error_count(result) == 1
      assert error_msg(result) =~ "Only one of 'id' or 'type'"

      assert result.data == %{
               "bot" => %{
                 "id" => bot.id,
                 "title" => bot.title,
                 "owner" => %{
                   "id" => user.id
                 },
                 "subscribers" => nil
               }
             }
    end

    test "get bot subscribers without id or type", %{
      user: user,
      bot: bot
    } do
      result = run_query(@query, user, %{"id" => bot.id})

      assert error_count(result) == 1
      assert error_msg(result) =~ "At least one of 'id' or 'type'"

      assert result.data == %{
               "bot" => %{
                 "id" => bot.id,
                 "title" => bot.title,
                 "owner" => %{
                   "id" => user.id
                 },
                 "subscribers" => nil
               }
             }
    end
  end

  describe "items and images" do
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
  end

  defp add_lat_lon(%Bot{location: location} = bot) do
    {lat, lon} = GeoUtils.get_lat_lon(location)
    bot |> Map.put(:lat, lat) |> Map.put(:lon, lon)
  end

  defp stringify_keys(map) do
    Enum.into(map, %{}, fn {k, v} -> {to_string(k), v} end)
  end
end
