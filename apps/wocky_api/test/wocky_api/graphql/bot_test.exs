defmodule WockyAPI.GraphQL.BotTest do
  use WockyAPI.GraphQLCase, async: false

  alias Faker.Lorem
  alias Wocky.{Block, Bot, GeoUtils, Repo, User}
  alias Wocky.Bot.Item
  alias Wocky.Bot.Invitation
  alias Wocky.Repo.Factory
  alias Wocky.Repo.ID
  alias Wocky.Repo.Timestamp

  setup :common_setup

  describe "basic bot queries" do
    test "get a single bot", %{user: user, bot: bot} do
      query = """
      query ($id: UUID!) {
        bot (id: $id) {
          id
          server
          createdAt
          updatedAt
        }
      }
      """

      result = run_query(query, user, %{"id" => bot.id})

      refute has_errors(result)

      assert result.data == %{
               "bot" => %{
                 "id" => bot.id,
                 "server" => Wocky.host(),
                 "createdAt" => Timestamp.to_string(bot.created_at),
                 "updatedAt" => Timestamp.to_string(bot.updated_at)
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
    query {
      currentUser {
        bots (first: 1, relationship: SUBSCRIBED_NOT_OWNED) {
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

    test "get subscribed but not owned bots", %{user: user, bot2: bot2} do
      Bot.subscribe(bot2, user)

      result = run_query(@query, user)

      refute has_errors(result)

      assert result.data == %{
               "currentUser" => %{
                 "bots" => %{
                   "totalCount" => 1,
                   "edges" => [
                     %{"node" => %{"id" => bot2.id}}
                   ]
                 }
               }
             }
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

    test "get bots anonymously", %{user2: user2} do
      result = run_query(@query, nil, %{"id" => user2.id})

      assert has_errors(result)

      assert error_msg(result) =~ "requires an authenticated user"
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

    @query """
    mutation ($id: UUID!) {
      botDelete (input: {id: $id}) {
        result
      }
    }
    """
    test "delete a bot", %{user: user, bot: bot} do
      result = run_query(@query, user, %{"id" => bot.id})

      refute has_errors(result)

      assert result.data == %{
               "botDelete" => %{
                 "result" => true
               }
             }

      assert Bot.get(bot.id) == nil
    end

    test "delete a non-owned bot", %{user: user, bot2: bot} do
      result = run_query(@query, user, %{"id" => bot.id})

      assert error_msg(result) == "Operation only permitted on owned bots"

      refute Bot.get(bot.id) == nil
    end

    test "delete a non-existant bot", %{user: user} do
      result = run_query(@query, user, %{"id" => ID.new()})

      assert error_msg(result) =~ "Bot not found"
    end
  end

  describe "active bots" do
    setup %{user: user, bot: bot, user2: user2, bot2: bot2} do
      Bot.subscribe(bot, user)
      Bot.subscribe(bot2, user)
      Bot.visit(bot, user, false)

      Bot.subscribe(bot2, user2)
      Bot.visit(bot2, user2, false)

      for b <- Factory.insert_list(3, :bot) do
        Bot.subscribe(b, user)
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

  describe "local bots" do
    setup %{user: user, user2: user2} do
      Repo.delete_all(Bot)

      {owned, subscribed, unrelated} =
        Enum.reduce(1..4, {[], [], []}, fn x, {o, s, u} ->
          loc = GeoUtils.point(x, x)
          owned = Factory.insert(:bot, user: user, location: loc)
          Bot.subscribe(owned, user)
          subscribed = Factory.insert(:bot, user: user2, location: loc)
          Bot.subscribe(subscribed, user)
          unrelated = Factory.insert(:bot, user: user2, location: loc)
          {[owned.id | o], [subscribed.id | s], [unrelated.id | u]}
        end)

      {:ok,
       owned: Enum.reverse(owned),
       subscribed: Enum.reverse(subscribed),
       unrelated: Enum.reverse(unrelated)}
    end

    @query """
    query ($pointA: Point!, $pointB: Point!) {
      localBots (pointA: $pointA, pointB: $pointB) {
        id
      }
    }
    """

    test "basic local bots", %{
      user: user,
      owned: owned,
      subscribed: subscribed,
      unrelated: unrelated
    } do
      result =
        run_query(@query, user, %{
          "pointA" => point_arg(0.0, 0.0),
          "pointB" => point_arg(5.0, 5.0)
        })

      refute has_errors(result)

      %{"localBots" => local_bots} = result.data

      assert length(local_bots) == 8

      ids = Enum.map(local_bots, &Map.get(&1, "id"))

      assert Enum.all?(ids, &Enum.member?(owned ++ subscribed, &1))
      refute Enum.any?(ids, &Enum.member?(unrelated, &1))
    end

    test "restricted area local bots", %{
      user: user,
      owned: [_, o | _],
      subscribed: [_, s | _]
    } do
      result =
        run_query(@query, user, %{
          "pointA" => point_arg(1.5, 1.5),
          "pointB" => point_arg(2.5, 2.5)
        })

      refute has_errors(result)

      %{"localBots" => local_bots} = result.data

      assert length(local_bots) == 2

      ids = Enum.map(local_bots, &Map.get(&1, "id"))

      assert Enum.all?(ids, &Enum.member?([o, s], &1))
    end
  end

  describe "bot mutations" do
    setup :require_watcher
    setup :common_setup

    @query """
    mutation {
      botCreate {
        successful
        result {
          id
          owner {
            id
          }
        }
      }
    }
    """
    test "preallocate bot", %{user: %{id: user_id} = user} do
      result = run_query(@query, user)

      assert %{
               "botCreate" => %{
                 "successful" => true,
                 "result" => %{
                   "id" => id,
                   "owner" => %{
                     "id" => ^user_id
                   }
                 }
               }
             } = result.data

      assert %Bot{pending: true, user_id: ^user_id} = Bot.get(id, true)
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
    test "create bot", %{user: user} do
      fields = [:title, :server, :lat, :lon, :radius, :description, :shortname]
      bot = :bot |> Factory.build() |> add_bot_lat_lon() |> Map.take(fields)

      result = run_query(@query, user, %{"values" => stringify_keys(bot)})

      refute has_errors(result)

      assert %{
               "botCreate" => %{
                 "successful" => true,
                 "result" => %{
                   "id" => id
                 }
               }
             } = result.data

      assert ^bot = id |> Bot.get() |> add_bot_lat_lon() |> Map.take(fields)
    end

    test "create bot with location", %{user: %{id: user_id} = user} do
      bot =
        :bot
        |> Factory.build()
        |> add_bot_lat_lon()
        |> Map.take(bot_create_fields())

      result =
        run_query(@query, user, %{
          "values" => stringify_keys(bot),
          "user_location" => %{
            "lat" => bot.lat,
            "lon" => bot.lon,
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

      bot = Bot.get(id)
      assert [%{id: ^user_id}] = bot |> Bot.visitors_query() |> Repo.all()
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
    test "update bot", %{user: user, bot: bot} do
      new_title = Lorem.sentence()

      result =
        run_query(@query, user, %{
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

    test "update pending bot", %{user: user} do
      bot = Bot.preallocate(user.id)

      values =
        :bot
        |> Factory.build()
        |> add_bot_lat_lon()
        |> Map.take(bot_create_fields())
        |> stringify_keys()

      result =
        run_query(@query, user, %{
          "id" => bot.id,
          "values" => values
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

      assert values["title"] == Bot.get(bot.id).title
    end

    test "update bot with location", %{user: %{id: user_id} = user, bot: bot} do
      new_title = Lorem.sentence()

      assert [] = bot |> Bot.visitors_query() |> Repo.all()

      result =
        run_query(@query, user, %{
          "id" => bot.id,
          "values" => %{"title" => new_title},
          "user_location" => %{
            "lat" => Bot.lat(bot),
            "lon" => Bot.lon(bot),
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

      assert new_title == Bot.get(bot.id).title
      assert [%{id: ^user_id}] = bot |> Bot.visitors_query() |> Repo.all()
    end
  end

  describe "bot subscriptions" do
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

    test "subscribe", %{user: user, bot2: bot2} do
      result = run_query(@query, user, %{"id" => bot2.id})

      refute has_errors(result)

      assert result.data == %{
               "botSubscribe" => %{"result" => true, "messages" => []}
             }

      assert Bot.subscription(bot2, user) == :subscribed
    end

    test "subscribe to a non-existent bot", %{user: user} do
      result = run_query(@query, user, %{"id" => ID.new()})

      assert error_count(result) == 1
      assert error_msg(result) =~ "Bot not found"
      assert result.data == %{"botSubscribe" => nil}
    end

    test "subscribe with location inside bot", %{user: user, bot2: bot2} do
      result =
        run_query(@query, user, %{
          "id" => bot2.id,
          "guest" => true,
          "user_location" => %{
            "lat" => Bot.lat(bot2),
            "lon" => Bot.lon(bot2),
            "accuracy" => 1,
            "device" => Lorem.word()
          }
        })

      refute has_errors(result)

      assert result.data == %{
               "botSubscribe" => %{"result" => true, "messages" => []}
             }

      assert Bot.subscription(bot2, user) == :visiting
    end

    test "subscribe with location outside bot", %{user: user, bot2: bot2} do
      result =
        run_query(@query, user, %{
          "id" => bot2.id,
          "guest" => true,
          "user_location" => %{
            "lat" => Bot.lat(bot2) + 5.0,
            "lon" => Bot.lon(bot2) + 5.0,
            "accuracy" => 1,
            "device" => Lorem.word()
          }
        })

      refute has_errors(result)

      assert result.data == %{
               "botSubscribe" => %{"result" => true, "messages" => []}
             }

      assert Bot.subscription(bot2, user) == :subscribed
    end

    test "subscribe with invalid location", %{user: user, bot2: bot2} do
      result =
        run_query(@query, user, %{
          "id" => bot2.id,
          "guest" => true,
          "user_location" => %{
            "lat" => Bot.lat(bot2),
            "lon" => Bot.lon(bot2),
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
      Bot.subscribe(bot, user2)
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
                       "relationships" => [
                         "GUEST",
                         "SUBSCRIBED",
                         "OWNED",
                         "VISIBLE"
                       ],
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
          "type" => "SUBSCRIBER"
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
    setup %{bot2: bot2, user: user, user2: user2} do
      item = Factory.insert(:item, bot: bot2, user: user)
      item2 = Factory.insert(:item, bot: bot2, user: user2)
      {:ok, item: item, item2: item2}
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
      image = Factory.insert(:tros_metadata, user: user)
      tros_url = Factory.image_url(image)
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

    @query """
    mutation ($input: BotItemPublishInput!) {
      botItemPublish (input: $input) {
        result {
          id
        }
      }
    }
    """
    test "publish item", %{user: user, bot: bot} do
      stanza = Lorem.paragraph()

      result =
        run_query(@query, user, %{
          "input" => %{
            "bot_id" => bot.id,
            "values" => %{"stanza" => stanza}
          }
        })

      refute has_errors(result)
      assert %{"botItemPublish" => %{"result" => %{"id" => id}}} = result.data
      assert %Item{stanza: ^stanza, id: ^id} = Item.get(id, bot)
    end

    test "update existing item", %{user: user, bot2: bot2, item: item} do
      id = item.id
      stanza = Lorem.paragraph()

      result =
        run_query(@query, user, %{
          "input" => %{
            "bot_id" => bot2.id,
            "values" => %{"id" => id, "stanza" => stanza}
          }
        })

      refute has_errors(result)
      assert %Item{stanza: ^stanza, id: ^id} = Item.get(id, bot2)
    end

    test "publish item failure", %{user: user, user2: user2, bot2: bot2} do
      result =
        run_query(@query, user, %{
          "input" => %{
            "bot_id" => bot2.id,
            "values" => %{"stanza" => Lorem.paragraph()}
          }
        })

      %{"botItemPublish" => %{"result" => %{"id" => id}}} = result.data

      result =
        run_query(@query, user2, %{
          "input" => %{
            "bot_id" => bot2.id,
            "values" => %{"id" => id, "stanza" => Lorem.paragraph()}
          }
        })

      assert error_msg(result) =~ "Permission denied"
    end

    @query """
    mutation ($input: BotItemDeleteInput!) {
      botItemDelete (input: $input) {
        result
      }
    }
    """
    test "delete own item", %{user: user, bot2: bot2, item: item} do
      result =
        run_query(@query, user, %{
          "input" => %{"bot_id" => bot2.id, "id" => item.id}
        })

      refute has_errors(result)

      assert result.data == %{"botItemDelete" => %{"result" => true}}
    end

    test "delete unowned item", %{user: user, bot2: bot2, item2: item2} do
      result =
        run_query(@query, user, %{
          "input" => %{"bot_id" => bot2.id, "id" => item2.id}
        })

      assert error_msg(result) =~ "Permission denied"
    end

    test "delete unowned item on owned bot", %{
      user: user2,
      bot2: bot2,
      item: item
    } do
      result =
        run_query(@query, user2, %{
          "input" => %{"bot_id" => bot2.id, "id" => item.id}
        })

      refute has_errors(result)

      assert result.data == %{"botItemDelete" => %{"result" => true}}
    end

    test "delete non-existant item", %{user: user, bot2: bot2} do
      result =
        run_query(@query, user, %{
          "input" => %{"bot_id" => bot2.id, "id" => ID.new()}
        })

      assert error_msg(result) =~ "Item not found"
    end

    test "delete on non-existant bot", %{user: user} do
      result =
        run_query(@query, user, %{
          "input" => %{"bot_id" => ID.new(), "id" => ID.new()}
        })

      assert error_msg(result) =~ "Bot not found"
    end
  end

  describe "sending invitations" do
    @query """
    mutation ($input: BotInviteInput!) {
      botInvite (input: $input) {
        successful
        messages {
          message
        }
        result {
          id
          accepted
        }
      }
    }
    """
    test "invite a user to a bot", %{bot: bot, user: user, user3: user3} do
      result =
        run_query(@query, user, %{
          "input" => %{"bot_id" => bot.id, "user_ids" => [user3.id]}
        })

      refute has_errors(result)

      assert %{"botInvite" => [%{"result" => %{"id" => id, "accepted" => nil}}]} =
               result.data

      assert %Invitation{accepted: nil} = Repo.get_by(Invitation, id: id)
    end

    test "invite a user to an unowned bot",
         %{bot2: bot2, user: user, user3: user3} do
      result =
        run_query(@query, user, %{
          "input" => %{"bot_id" => bot2.id, "user_ids" => [user3.id]}
        })

      assert error_msg(result) =~ "Invalid bot"
    end

    test "invite a blocked user", %{bot: bot, user: user, user3: user3} do
      Block.block(user3, user)

      result =
        run_query(@query, user, %{
          "input" => %{"bot_id" => bot.id, "user_ids" => [user3.id]}
        })

      r = hd(result.data["botInvite"])
      assert failure_msg(r) =~ "Invalid user"
      refute successful?(r)
    end

    test "multiple invitations", %{bot: bot, user: user, user3: user3} do
      user4 = Factory.insert(:user)
      Block.block(user4, user)

      result =
        run_query(@query, user, %{
          "input" => %{"bot_id" => bot.id, "user_ids" => [user3.id, user4.id]}
        })

      refute has_errors(result)

      [r1, r2] = result.data["botInvite"]

      assert %{"result" => %{"id" => id}} = r1
      assert successful?(r1)
      assert %Invitation{accepted: nil} = Repo.get_by(Invitation, id: id)

      assert failure_msg(r2) =~ "Invalid user"
      refute successful?(r2)
    end

    test "gives users access to the bot", shared do
      refute User.can_access?(shared.user3, shared.bot)

      Factory.insert(:invitation,
        user: shared.user,
        bot: shared.bot,
        invitee: shared.user3
      )

      assert User.can_access?(shared.user3, shared.bot)
    end
  end

  describe "responding to invitations" do
    setup shared do
      %{id: id} =
        Factory.insert(:invitation,
          user: shared.user,
          bot: shared.bot,
          invitee: shared.user3
        )

      {:ok, id: id}
    end

    @query """
    mutation ($input: BotInvitationRespondInput) {
      botInvitationRespond (input: $input) {
        result
      }
    }
    """

    test "accepting", %{id: id} = shared do
      result =
        run_query(@query, shared.user3, %{
          "input" => %{"invitation_id" => to_string(id), "accept" => true}
        })

      refute has_errors(result)

      assert %Invitation{accepted: true} = Repo.get_by(Invitation, id: id)
      assert User.can_access?(shared.user3, shared.bot)
    end

    test "accepting with location", %{id: id} = shared do
      {lat, lon} = GeoUtils.get_lat_lon(shared.bot.location)

      result =
        run_query(@query, shared.user3, %{
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

      user_id = shared.user3.id

      assert [%{id: ^user_id}] =
               shared.bot |> Bot.visitors_query() |> Repo.all()
    end

    test "declining", %{id: id} = shared do
      result =
        run_query(@query, shared.user3, %{
          "input" => %{"invitation_id" => to_string(id), "accept" => false}
        })

      refute has_errors(result)

      assert %Invitation{accepted: false} = Repo.get_by(Invitation, id: id)
      refute Bot.subscription(shared.bot, shared.user3)
    end

    test "can't accept an invitation to someone else", shared do
      user4 = Factory.insert(:user)

      %{id: id} =
        Factory.insert(:invitation,
          user: shared.user,
          bot: shared.bot,
          invitee: user4
        )

      result =
        run_query(@query, shared.user, %{
          "input" => %{"invitation_id" => to_string(id), "accept" => true}
        })

      assert error_msg(result) =~ "Permission denied"
    end
  end

  defp common_setup(_) do
    [user, user2, user3] = Factory.insert_list(3, :user)

    image = Factory.insert(:tros_metadata, user: user)
    bot = Factory.insert(:bot, image: Factory.image_url(image), user: user)
    bot2 = Factory.insert(:bot, user: user2)
    Factory.insert(:subscription, bot: bot, user: user2)
    Factory.insert(:subscription, bot: bot2, user: user)

    {:ok, user: user, user2: user2, user3: user3, bot: bot, bot2: bot2}
  end

  defp point_arg(lat, lon), do: %{"lat" => lat, "lon" => lon}
end
