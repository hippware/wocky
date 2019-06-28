defmodule WockyAPI.GraphQL.BotTest do
  use WockyAPI.GraphQLCase, async: true

  alias Faker.Lorem
  alias Wocky.Account.User
  alias Wocky.Block
  alias Wocky.GeoUtils
  alias Wocky.POI
  alias Wocky.POI.Bot
  alias Wocky.POI.Item
  alias Wocky.Relation
  alias Wocky.Relation.Invitation
  alias Wocky.Repo
  alias Wocky.Repo.Factory
  alias Wocky.Repo.ID
  alias Wocky.Repo.Timestamp
  alias Wocky.Roster
  alias WockyAPI.Factory, as: APIFactory

  setup do
    [user, user2, user3] = Factory.insert_list(3, :user)

    image = Factory.insert(:tros_metadata, user: user)
    image_url = APIFactory.image_url(image)
    bot = Factory.insert(:bot, image_url: image_url, user: user)
    bot2 = Factory.insert(:bot, user: user2)
    Factory.insert(:subscription, bot: bot, user: user2)
    Factory.insert(:subscription, bot: bot2, user: user)

    {:ok, user: user, user2: user2, user3: user3, bot: bot, bot2: bot2}
  end

  describe "basic bot queries" do
    test "get a single bot", %{user: user, bot: bot} do
      query = """
      query ($id: UUID!) {
        bot (id: $id) {
          id
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
                 "createdAt" => Timestamp.to_string!(bot.created_at),
                 "updatedAt" => Timestamp.to_string!(bot.updated_at)
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
      Relation.subscribe(user, bot2)

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
  end

  describe "paging queries" do
    @query """
    query ($first: Int, $last: Int,
           $before: String, $after: String) {
      currentUser {
        bots (first: $first, last: $last, before: $before,
              after: $after, relationship: OWNED) {
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

    setup %{user: user, bot: bot} do
      bots =
        [bot | Factory.insert_list(10, :bot, user: user)]
        |> Enum.reverse()
        |> Enum.map(& &1.id)

      {:ok, bots: bots}
    end

    test "get last items in a list (no cursor)", %{user: user, bots: bots} do
      [b8_id, b9_id, b10_id] = bots |> Enum.slice(-3..-1)

      result! = run_query(@query, user, %{"last" => 3})

      refute has_errors(result!)

      assert %{
               "currentUser" => %{
                 "bots" => %{
                   "totalCount" => 11,
                   "edges" => [
                     %{
                       "cursor" => _c8,
                       "node" => %{
                         "id" => ^b8_id
                       }
                     },
                     %{
                       "cursor" => _c9,
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
             } = result!.data
    end

    test "get first items in a list (no cursor)", %{user: user, bots: bots} do
      [b0_id, b1_id] = bots |> Enum.take(2)

      result = run_query(@query, user, %{"first" => 2})

      refute has_errors(result)

      assert %{
               "currentUser" => %{
                 "bots" => %{
                   "totalCount" => 11,
                   "edges" => [
                     %{
                       "cursor" => _c0,
                       "node" => %{
                         "id" => ^b0_id
                       }
                     },
                     %{
                       "cursor" => _c1,
                       "node" => %{
                         "id" => ^b1_id
                       }
                     }
                   ],
                   "pageInfo" => %{
                     "hasNextPage" => true,
                     "hasPreviousPage" => false
                   }
                 }
               }
             } = result.data
    end

    test "last/before", %{user: user, bots: bots} do
      result! =
        run_query(@query, user, %{
          "last" => 2,
          "before" => cursor_for(user, 9)
        })

      refute has_errors(result!)

      b7_id = id(bots, 7)
      b8_id = id(bots, 8)

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
                       "cursor" => _c8,
                       "node" => %{
                         "id" => ^b8_id
                       }
                     }
                   ],
                   "pageInfo" => %{
                     "hasNextPage" => false,
                     "hasPreviousPage" => true
                   }
                 }
               }
             } = result!.data
    end

    test "first/after", %{user: user, bots: bots} do
      result =
        run_query(@query, user, %{
          "first" => 2,
          "after" => cursor_for(user, 3)
        })

      refute has_errors(result)

      b4_id = id(bots, 4)
      b5_id = id(bots, 5)

      assert %{
               "currentUser" => %{
                 "bots" => %{
                   "totalCount" => 11,
                   "edges" => [
                     %{
                       "cursor" => _c4,
                       "node" => %{
                         "id" => ^b4_id
                       }
                     },
                     %{
                       "cursor" => _c5,
                       "node" => %{
                         "id" => ^b5_id
                       }
                     }
                   ],
                   "pageInfo" => %{
                     "hasNextPage" => true,
                     "hasPreviousPage" => false
                   }
                 }
               }
             } = result.data
    end

    test "last/after", %{user: user, bots: bots} do
      result =
        run_query(@query, user, %{
          "last" => 2,
          "after" => cursor_for(user, 4)
        })

      refute has_errors(result)

      b9_id = id(bots, 9)
      b10_id = id(bots, 10)

      assert %{
               "currentUser" => %{
                 "bots" => %{
                   "totalCount" => 11,
                   "edges" => [
                     %{
                       "cursor" => _c9,
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
    end

    test "first/before", %{user: user, bots: bots} do
      result =
        run_query(@query, user, %{
          "first" => 2,
          "before" => cursor_for(user, 6)
        })

      refute has_errors(result)

      b0_id = id(bots, 0)
      b1_id = id(bots, 1)

      assert %{
               "currentUser" => %{
                 "bots" => %{
                   "totalCount" => 11,
                   "edges" => [
                     %{
                       "cursor" => _c0,
                       "node" => %{
                         "id" => ^b0_id
                       }
                     },
                     %{
                       "cursor" => _c1,
                       "node" => %{
                         "id" => ^b1_id
                       }
                     }
                   ],
                   "pageInfo" => %{
                     "hasNextPage" => true,
                     "hasPreviousPage" => false
                   }
                 }
               }
             } = result.data
    end

    test "last/before limit", %{user: user, bots: bots} do
      result =
        run_query(@query, user, %{
          "last" => 2,
          "before" => cursor_for(user, 1)
        })

      refute has_errors(result)

      b0_id = id(bots, 0)

      assert %{
               "currentUser" => %{
                 "bots" => %{
                   "totalCount" => 11,
                   "edges" => [
                     %{
                       "cursor" => _c0,
                       "node" => %{
                         "id" => ^b0_id
                       }
                     }
                   ],
                   "pageInfo" => %{
                     "hasNextPage" => false,
                     "hasPreviousPage" => false
                   }
                 }
               }
             } = result.data
    end

    test "first/after limit", %{user: user, bots: bots} do
      result =
        run_query(@query, user, %{
          "first" => 3,
          "after" => cursor_for(user, 8)
        })

      refute has_errors(result)

      b9_id = id(bots, 9)
      b10_id = id(bots, 10)

      assert %{
               "currentUser" => %{
                 "bots" => %{
                   "totalCount" => 11,
                   "edges" => [
                     %{
                       "cursor" => _c9,
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
                     "hasPreviousPage" => false
                   }
                 }
               }
             } = result.data
    end

    test "last/after limit", %{user: user, bots: bots} do
      result =
        run_query(@query, user, %{
          "last" => 3,
          "after" => cursor_for(user, 8)
        })

      refute has_errors(result)

      b9_id = id(bots, 9)
      b10_id = id(bots, 10)

      assert %{
               "currentUser" => %{
                 "bots" => %{
                   "totalCount" => 11,
                   "edges" => [
                     %{
                       "cursor" => _c9,
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
    end

    test "first/before limit", %{user: user, bots: bots} do
      result =
        run_query(@query, user, %{
          "first" => 3,
          "before" => cursor_for(user, 2)
        })

      refute has_errors(result)

      b0_id = id(bots, 0)
      b1_id = id(bots, 1)

      assert %{
               "currentUser" => %{
                 "bots" => %{
                   "totalCount" => 11,
                   "edges" => [
                     %{
                       "cursor" => _c0,
                       "node" => %{
                         "id" => ^b0_id
                       }
                     },
                     %{
                       "cursor" => _c1,
                       "node" => %{
                         "id" => ^b1_id
                       }
                     }
                   ],
                   "pageInfo" => %{
                     "hasNextPage" => true,
                     "hasPreviousPage" => false
                   }
                 }
               }
             } = result.data
    end

    defp cursor_for(user, i) do
      result = run_query(@query, user, %{"first" => 100})

      refute has_errors(result)

      %{"currentUser" => %{"bots" => %{"edges" => edges}}} = result.data
      Enum.at(edges, i)["cursor"]
    end

    defp id(bots, i), do: Enum.at(bots, i)
  end

  describe "bot deletion" do
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

      assert POI.get(bot.id) == nil
    end

    test "delete a non-owned bot", %{user: user, bot2: bot} do
      result = run_query(@query, user, %{"id" => bot.id})

      assert error_msg(result) == "Operation only permitted on owned bots"

      refute POI.get(bot.id) == nil
    end

    test "delete a non-existant bot", %{user: user} do
      result = run_query(@query, user, %{"id" => ID.new()})

      assert error_msg(result) =~ "Bot not found"
    end
  end

  describe "active bots" do
    setup %{user: user, bot: bot, user2: user2, bot2: bot2} do
      Relation.subscribe(user, bot)
      Relation.subscribe(user, bot2)
      Relation.visit(user, bot, false)

      Relation.subscribe(user2, bot2)
      Relation.visit(user2, bot2, false)

      for b <- Factory.insert_list(3, :bot) do
        Relation.subscribe(user, b)
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
      Roster.befriend(user, user2)

      {owned, subscribed, unrelated} =
        Enum.reduce(1..4, {[], [], []}, fn x, {o, s, u} ->
          loc = GeoUtils.point(x, x)
          owned = Factory.insert(:bot, user: user, location: loc)
          Relation.subscribe(user, owned)
          subscribed = Factory.insert(:bot, user: user2, location: loc)
          Relation.subscribe(user, subscribed)
          unrelated = Factory.insert(:bot, user: user2, location: loc)
          {[owned.id | o], [subscribed.id | s], [unrelated.id | u]}
        end)

      Application.put_env(:wocky, :max_local_bots_search_radius, 1_000_000)

      {:ok,
       owned: Enum.reverse(owned),
       subscribed: Enum.reverse(subscribed),
       unrelated: Enum.reverse(unrelated)}
    end

    @query """
    query ($pointA: Point!, $pointB: Point!, $limit: Int) {
      localBots (pointA: $pointA, pointB: $pointB, limit: $limit) {
        bots { id }
        areaTooLarge
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

      %{"localBots" => %{"areaTooLarge" => false, "bots" => local_bots}} =
        result.data

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

      %{"localBots" => %{"areaTooLarge" => false, "bots" => local_bots}} =
        result.data

      assert length(local_bots) == 2

      ids = Enum.map(local_bots, &Map.get(&1, "id"))

      assert Enum.all?(ids, &Enum.member?([o, s], &1))
    end

    test "limit returned bots", %{
      user: user,
      owned: owned,
      subscribed: subscribed
    } do
      result =
        run_query(@query, user, %{
          "pointA" => point_arg(0.0, 0.0),
          "pointB" => point_arg(5.0, 5.0),
          "limit" => 2
        })

      refute has_errors(result)

      %{"localBots" => %{"areaTooLarge" => false, "bots" => local_bots}} =
        result.data

      assert length(local_bots) == 2

      ids = Enum.map(local_bots, &Map.get(&1, "id"))
      assert ids == [List.last(subscribed), List.last(owned)]
    end

    test "exceed search area", %{user: user} do
      result =
        run_query(@query, user, %{
          "pointA" => point_arg(0.0, 0.0),
          "pointB" => point_arg(10.0, 10.0)
        })

      refute has_errors(result)

      assert %{"localBots" => %{"areaTooLarge" => true, "bots" => []}} ==
               result.data
    end

    test "search area straddling 180th meridian", ctx do
      loc = GeoUtils.point(0.0, -179.0)
      bot = Factory.insert(:bot, user: ctx.user, location: loc)
      Relation.subscribe(ctx.user, bot)

      result =
        run_query(@query, ctx.user, %{
          "pointA" => point_arg(1.0, 178.0),
          "pointB" => point_arg(-1.0, -178.0)
        })

      refute has_errors(result)

      %{"localBots" => %{"areaTooLarge" => false, "bots" => local_bots}} =
        result.data

      assert length(local_bots) == 1

      assert hd(local_bots)["id"] == bot.id
    end
  end

  describe "localBotsCluster search" do
    @query """
    query ($pointA: Point!, $pointB: Point!, $latDivs: Int!, $lonDivs: Int!) {
      localBotsCluster (pointA: $pointA, pointB: $pointB,
                        latDivs: $latDivs, lonDivs: $lonDivs) {
        bots { id }
        clusters {
          count
          lat
          lon
        }
        areaTooLarge
      }
    }
    """
    setup ctx do
      Application.put_env(:wocky, :max_local_bots_search_radius, 1_000_000)

      locations = [
        {1.5, 1.5},
        {0.1, 0.1},
        {0.2, 0.2},
        {0.3, 0.3},
        {0.67, 0.67},
        {0.68, 0.68},
        {3.0, 3.0}
      ]

      b =
        Enum.map(locations, fn {lat, lon} ->
          l = GeoUtils.point(lat, lon)
          Factory.insert(:bot, location: l, user: ctx.user)
        end)

      Enum.each(b, &Relation.subscribe(ctx.user, &1))

      {:ok, bots: b}
    end

    test "localBotsCluster search", ctx do
      result =
        run_query(@query, ctx.user, %{
          "pointA" => point_arg(0.0, 0.0),
          "pointB" => point_arg(2.0, 2.0),
          "latDivs" => 3,
          "lonDivs" => 3
        })

      refute has_errors(result)

      %{
        "localBotsCluster" => %{
          "areaTooLarge" => false,
          "bots" => bots,
          "clusters" => clusters
        }
      } = result.data

      assert length(bots) == 1
      assert hd(bots)["id"] == hd(ctx.bots).id

      assert length(clusters) == 2

      assert has_clusters(clusters, [
               {2, 1.0, 1.0},
               {3, 0.3333333333333333, 0.3333333333333333}
             ])
    end

    defp has_clusters(clusters, expected) do
      expected
      |> Enum.map(fn {count, lat, lon} ->
        %{"count" => count, "lat" => lat, "lon" => lon}
      end)
      |> Enum.sort()
      |> Enum.zip(Enum.sort(clusters))
      |> Enum.all?(fn {x, y} -> x == y end)
    end
  end

  describe "bot mutations" do
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

      assert %Bot{pending: true, user_id: ^user_id} = POI.get(id, true)
    end

    @query """
    mutation ($values: BotParams) {
      botCreate (input: {values: $values}) {
        successful
        result {
          id
        }
      }
    }
    """
    test "create bot", %{user: user} do
      refute Repo.get(User, user.id).bot_created

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

      assert ^bot = id |> POI.get() |> add_bot_lat_lon() |> Map.take(fields)

      assert Repo.get(User, user.id).bot_created
    end

    @query """
    mutation ($id: UUID!, $values: BotParams!) {
      botUpdate (input: {id: $id, values: $values}) {
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

      assert new_title == POI.get(bot.id).title
    end

    test "update pending bot", %{user: user} do
      bot = POI.preallocate(user)

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

      assert values["title"] == POI.get(bot.id).title
    end
  end

  describe "bot subscriptions" do
    @query """
    mutation ($id: UUID!, $guest: Boolean) {
      botSubscribe (input: {id: $id, guest: $guest}) {
        result
        messages  {
          field
          message
        }
      }
    }
    """

    setup ctx do
      Roster.befriend(ctx.user, ctx.user2)
      unsubbed_bot = Factory.insert(:bot, user: ctx.user2)

      Factory.insert(:bot_invitation,
        user: ctx.user2,
        invitee: ctx.user,
        bot: unsubbed_bot
      )

      {:ok, unsubbed_bot: unsubbed_bot}
    end

    test "subscribe", %{user: user, unsubbed_bot: bot} do
      result = run_query(@query, user, %{"id" => bot.id})

      refute has_errors(result)

      assert result.data == %{
               "botSubscribe" => %{"result" => true, "messages" => []}
             }

      assert Relation.subscribed?(user, bot)
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
      Relation.subscribe(user, bot2)

      result = run_query(@query, user, %{"id" => bot2.id})

      refute has_errors(result)
      assert result.data == %{"botUnsubscribe" => %{"result" => true}}
      refute Relation.subscribed?(user, bot2)
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
      Relation.subscribe(user2, bot)

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

    test "get bot visitors", %{bot: bot, user: user, user2: user2} do
      Relation.subscribe(user2, bot)
      Relation.visit(user2, bot, false)

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
      Relation.subscribe(user, bot)
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

    test "bot image", %{bot: %{id: id, image_url: image_url}, user: user} do
      query = """
      query ($id: UUID) {
        bot (id: $id) {
          media {
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
                 "media" => %{
                   "tros_url" => ^image_url,
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
              content
              media {
                tros_url
                full_url
                thumbnail_url
              }
              owner {
                id
              }
            }
          }
        }
      }
    }
    """

    test "bot item with content and image", %{bot: bot, user: user} do
      image = Factory.insert(:tros_metadata, user: user)
      tros_url = APIFactory.image_url(image)
      item = Factory.insert(:item, user: user, bot: bot, image_url: tros_url)

      result = run_query(@query, user, %{"id" => bot.id})

      refute has_errors(result)

      user_id = user.id
      content = item.content

      assert %{
               "bot" => %{
                 "items" => %{
                   "edges" => [
                     %{
                       "node" => %{
                         "content" => ^content,
                         "media" => %{
                           "tros_url" => ^tros_url,
                           "full_url" => "https://" <> _,
                           "thumbnail_url" => "https://" <> _
                         },
                         "owner" => %{"id" => ^user_id}
                       }
                     }
                   ]
                 }
               }
             } = result.data
    end

    test "bot item with content and no image", %{bot: bot, user: user} do
      %{content: content} = Factory.insert(:item, bot: bot)

      result = run_query(@query, user, %{"id" => bot.id})

      refute has_errors(result)

      assert %{
               "bot" => %{
                 "items" => %{
                   "edges" => [
                     %{
                       "node" => %{
                         "content" => ^content,
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
      content = Lorem.paragraph()
      image_url = "tros://testing"

      result =
        run_query(@query, user, %{
          "input" => %{
            "bot_id" => bot.id,
            "content" => content,
            "imageUrl" => image_url
          }
        })

      refute has_errors(result)
      assert %{"botItemPublish" => %{"result" => %{"id" => id}}} = result.data

      assert %Item{id: ^id, content: ^content, image_url: ^image_url} =
               POI.get_item(bot, id)
    end

    test "update existing item", %{user: user, bot2: bot2, item: item} do
      id = item.id
      content = Lorem.paragraph()

      result =
        run_query(@query, user, %{
          "input" => %{
            "bot_id" => bot2.id,
            "id" => id,
            "content" => content
          }
        })

      refute has_errors(result)
      assert %Item{content: ^content, id: ^id} = POI.get_item(bot2, id)
    end

    test "publish item with invalid bot id", %{user: user} do
      result =
        run_query(@query, user, %{
          "input" => %{
            "bot_id" => ID.new(),
            "content" => Lorem.paragraph()
          }
        })

      assert error_msg(result) =~ "Bot not found"
    end

    test "publish item permission error", %{
      user: user,
      user2: user2,
      bot2: bot2
    } do
      result! =
        run_query(@query, user, %{
          "input" => %{
            "bot_id" => bot2.id,
            "content" => Lorem.paragraph()
          }
        })

      %{"botItemPublish" => %{"result" => %{"id" => id}}} = result!.data

      result! =
        run_query(@query, user2, %{
          "input" => %{
            "bot_id" => bot2.id,
            "id" => id,
            "content" => Lorem.paragraph()
          }
        })

      assert error_msg(result!) =~ "Permission denied"
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
    setup ctx do
      Roster.befriend(ctx.user, ctx.user3)

      :ok
    end

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

    test "invite a non-friend user",
         %{bot: bot, user: user, user2: user2} do
      result =
        run_query(@query, user, %{
          "input" => %{"bot_id" => bot.id, "user_ids" => [user2.id]}
        })

      r = hd(result.data["botInvite"])
      assert failure_msg(r) =~ "Permission denied"
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
      refute Relation.visible?(shared.user3, shared.bot)

      Factory.insert(:bot_invitation,
        user: shared.user,
        bot: shared.bot,
        invitee: shared.user3
      )

      assert Relation.visible?(shared.user3, shared.bot)
    end
  end

  describe "responding to invitations" do
    setup shared do
      Roster.befriend(shared.user, shared.user3)

      %{id: id} =
        Factory.insert(:bot_invitation,
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
      assert Relation.visible?(shared.user3, shared.bot)
    end

    test "declining", %{id: id} = shared do
      result =
        run_query(@query, shared.user3, %{
          "input" => %{"invitation_id" => to_string(id), "accept" => false}
        })

      refute has_errors(result)

      assert %Invitation{accepted: false} = Repo.get_by(Invitation, id: id)
      refute Relation.subscribed?(shared.user3, shared.bot)
    end

    test "can't accept an invitation to someone else", shared do
      user4 = Factory.insert(:user)

      %{id: id} =
        Factory.insert(:bot_invitation,
          user: shared.user,
          bot: shared.bot,
          invitee: user4
        )

      result =
        run_query(@query, shared.user, %{
          "input" => %{"invitation_id" => to_string(id), "accept" => true}
        })

      assert error_msg(result) =~ "Invalid invitation"
    end
  end

  defp point_arg(lat, lon), do: %{"lat" => lat, "lon" => lon}
end
