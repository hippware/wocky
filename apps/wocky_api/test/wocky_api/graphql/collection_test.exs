defmodule WockyAPI.GraphQL.CollectionTest do
  use WockyAPI.ConnCase, async: true

  alias Faker.Lorem
  alias Wocky.Account
  alias Wocky.Collections
  alias Wocky.Collections.Collection
  alias Wocky.Repo
  alias Wocky.Repo.Factory

  setup do
    [user, user2] = Factory.insert_list(2, :user)
    {:ok, {token, _}} = Account.assign_token(user.id, "abc")

    bots = Factory.insert_list(5, :bot, user: user, public: true)
    coll_bots = Factory.insert_list(5, :bot, user: user, public: true)
    collection = Factory.insert(:collection, user: user, title: Lorem.sentence())
    Enum.each(coll_bots, &Collections.add_bot(collection.id, &1.id, user))
    Collections.subscribe(collection.id, user2)

    conn =
      build_conn()
      |> put_req_header("x-auth-user", user.id)
      |> put_req_header("x-auth-token", token)

    {:ok,
      user: user,
      user2: user2,
      bots: bots,
      coll_bots: coll_bots,
      collection: collection,
      conn: conn}
  end

  @query """
  query ($id: AInt!) {
    collection (id: $id) {
      title
      owner {
        id
      }
      bots (first: 10) {
        totalCount
        edges {
          node {
            id
          }
        }
      }
      subscribers (first: 10) {
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
  test "get collection by id",
  %{conn: conn,
    user: %{id: user_id},
    user2: %{id: user2_id},
    collection: %{id: collection_id, title: title},
    coll_bots: coll_bots}
  do
    assert %{
      "data" => %{
        "collection" => %{
          "title" => ^title,
          "owner" => %{
            "id" => ^user_id
          },
          "bots" => %{
            "totalCount" => 5,
            "edges" => edges
          },
          "subscribers" => %{
            "totalCount" => 1,
            "edges" => [%{
              "node" => %{
                "id" => ^user2_id
              }
            }]
          }
        }
      }
    } =
      post_conn(conn, @query, %{id: Integer.to_string(collection_id)}, 200)

    bot_ids = Enum.map(edges, &(&1["node"]["id"]))
    assert Enum.sort(bot_ids) == ids(coll_bots)
  end

  @query """
  query ($id: UUID!) {
    bot (id: $id) {
      collections (first: 1) {
        totalCount
        edges {
          node {
            id
            title
            owner {
              id
            }
          }
        }
      }
    }
  }
  """
  test "get collection through bot",
  %{conn: conn,
    user: %{id: user_id},
    collection: %{id: collection_id, title: title},
    coll_bots: coll_bots}
  do
    assert post_conn(conn, @query, %{id: hd(coll_bots).id}, 200) ==
      %{
        "data" => %{
          "bot" => %{
            "collections" => %{
              "totalCount" => 1,
              "edges" => [%{
                "node" => %{
                  "id" => Integer.to_string(collection_id),
                  "title" => title,
                  "owner" => %{
                    "id" => user_id
                  }
                }
              }]
            }
          }
        }
      }
  end

  @query """
  {
    currentUser {
      collections (first: 1) {
        totalCount
        edges {
          node {
            id
            title
            bots (first: 0) {
              totalCount
            }
          }
        }
      }
    }
  }
  """
  test "get owned collection through current user",
  %{conn: conn,
    collection: %{id: collection_id, title: title},
    coll_bots: coll_bots}
  do
    assert post_conn(conn, @query, %{}, 200) ==
      %{
        "data" => %{
          "currentUser" => %{
            "collections" => %{
              "totalCount" => 1,
              "edges" => [%{
                "node" => %{
                  "id" => Integer.to_string(collection_id),
                  "title" => title,
                  "bots" => %{
                    "totalCount" => length(coll_bots)
                  }
                }
              }]
            }
          }
        }
      }
  end

  @query """
  query ($id: UUID!) {
    user (id: $id) {
      subscribedCollections (first: 1) {
        totalCount
        edges {
          node {
            id
            title
            bots (first: 0) {
              totalCount
            }
          }
        }
      }
    }
  }
  """
  test "get subscribed collection through user",
    %{conn: conn,
      user2: user,
      collection: %{id: collection_id, title: title},
      coll_bots: coll_bots}
  do
    assert post_conn(conn, @query, %{id: user.id}, 200) ==
      %{
        "data" => %{
          "user" => %{
            "subscribedCollections" => %{
              "totalCount" => 1,
              "edges" => [%{
                "node" => %{
                  "id" => Integer.to_string(collection_id),
                  "title" => title,
                  "bots" => %{
                    "totalCount" => length(coll_bots)
                  }
                }
              }]
            }
          }
        }
      }
  end

  @query """
  mutation ($input: CollectionCreateInput!) {
    collectionCreate (input: $input) {
      result {
        id
        title
        owner {
          id
        }
      }
    }
  }
  """
  test "create collection mutation",
  %{conn: conn, user: %{id: user_id} = user} do
    title = Lorem.sentence()
    assert %{
        "data" => %{
          "collectionCreate" => %{
            "result" => %{
              "id" => id,
              "title" => ^title,
              "owner" => %{
                "id" => ^user_id
              }
            }
          }
        }
      } =
    post_conn(conn, @query, %{input: %{title: title}}, 200)

    assert %Collection{title: ^title} =
      id
      |> String.to_integer()
      |> Collections.get_query(user)
      |> Repo.one!()
  end

  @query """
  mutation ($input: CollectionUpdateInput!) {
    collectionUpdate (input: $input) {
      result {
        title
      }
    }
  }
  """
  test "update collection mutation",
  %{conn: conn, collection: %{id: id}, user: user} do
    new_title = Lorem.sentence()
    assert %{
        "data" => %{
          "collectionUpdate" => %{
            "result" => %{
              "title" => ^new_title
            }
          }
        }
      } =
        post_conn(conn, @query,
                  %{input: %{id: Integer.to_string(id), title: new_title}}, 200)

    assert %Collection{title: ^new_title} =
      id
      |> Collections.get_query(user)
      |> Repo.one!()
  end


  @query """
  mutation ($input: CollectionDeleteInput!) {
    collectionDelete (input: $input) {
      result
    }
  }
  """
  test "delete collection mutation",
  %{conn: conn, collection: %{id: id}, user: user} do
    assert %{
        "data" => %{
          "collectionDelete" => %{
            "result" => true
          }
        }
      } =
    post_conn(conn, @query, %{input: %{id: Integer.to_string(id)}}, 200)

    assert nil ==
      id
      |> Collections.get_query(user)
      |> Repo.one()
  end

  @query """
  mutation ($id: AInt!) {
    collectionSubscribe (input: {id: $id}) {
      result
    }
  }
  """
  test "subscribe collection mutation", %{
    conn: conn,
    collection: %{id: id} = collection,
    user: user
  } do
    assert %{
             "data" => %{
               "collectionSubscribe" => %{
                 "result" => true
               }
             }
           } = post_conn(conn, @query, %{id: Integer.to_string(id)}, 200)

    assert collection
           |> Collections.get_subscribers_query(user)
           |> Repo.all()
           |> Enum.any?(&(&1.id == user.id))
  end

  @query """
  mutation ($id: AInt!) {
    collectionUnsubscribe (input: {id: $id}) {
      result
    }
  }
  """
  test "unsubscribe collection mutation", %{
    conn: conn,
    collection: %{id: id} = collection,
    user: user
  } do
    assert %{
             "data" => %{
               "collectionUnsubscribe" => %{
                 "result" => true
               }
             }
           } = post_conn(conn, @query, %{id: Integer.to_string(id)}, 200)

    assert collection
           |> Collections.get_subscribers_query(user)
           |> Repo.all()
           |> Enum.all?(&(&1.id != user.id))
  end

  @query """
  mutation ($id: UUID!, $bot_id: UUID!) {
    collectionAddBot (input: {id: $id, botId: $bot_id}) {
      result
    }
  }
  """
  test "add bot collection mutation", %{
    conn: conn,
    collection: %{id: id} = collection,
    user: user
  } do
    bot = Factory.insert(:bot, user: user, public: true)
    assert %{
             "data" => %{
               "collectionAddBot" => %{
                 "result" => true
               }
             }
           } =
             post_conn(
               conn,
               @query,
               %{id: to_string(id), bot_id: bot.id},
               200
             )

    assert collection
           |> Collections.get_members_query(user)
           |> Repo.all()
           |> Enum.any?(&(&1.id == bot.id))
  end

  @query """
  mutation ($id: UUID!, $bot_id: UUID!) {
    collectionRemoveBot (input: {id: $id, botId: $bot_id}) {
      result
    }
  }
  """
  test "remove bot collection mutation", %{
    conn: conn,
    collection: %{id: id} = collection,
    user: user,
    coll_bots: [bot | _]
  } do
    assert %{
             "data" => %{
               "collectionRemoveBot" => %{
                 "result" => true
               }
             }
           } =
             post_conn(
               conn,
               @query,
               %{id: to_string(id), bot_id: bot.id},
               200
             )

    assert collection
           |> Collections.get_members_query(user)
           |> Repo.all()
           |> Enum.all?(&(&1.id != bot.id))
  end

  defp ids(items), do: Enum.sort(Enum.map(items, &(&1.id)))
end
