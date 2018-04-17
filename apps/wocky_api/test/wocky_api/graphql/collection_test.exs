defmodule WockyAPI.GraphQL.CollectionTest do
  use WockyAPI.GraphQLCase, async: true

  import SweetXml

  alias Faker.Lorem
  alias Wocky.Collections
  alias Wocky.Collections.Collection
  alias Wocky.HomeStream
  alias Wocky.HomeStream.Item
  alias Wocky.Repo
  alias Wocky.Repo.Factory

  setup do
    [user, user2] = Factory.insert_list(2, :user)

    bots = Factory.insert_list(5, :bot, user: user, public: true)
    coll_bots = Factory.insert_list(5, :bot, user: user, public: true)

    collection =
      Factory.insert(:collection, user: user, title: Lorem.sentence())

    Enum.each(coll_bots, &Collections.add_bot(collection.id, &1.id, user))
    Collections.subscribe(collection.id, user2)

    {:ok,
     user: user,
     user2: user2,
     bots: bots,
     coll_bots: coll_bots,
     collection: collection}
  end

  test "get collection by id", %{
    user: %{id: user_id} = user,
    user2: %{id: user2_id},
    collection: %{id: collection_id, title: title},
    coll_bots: coll_bots
  } do
    data =
      """
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
      |> run_query(user, %{"id" => to_string(collection_id)})

    assert %{
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
                 "edges" => [
                   %{
                     "node" => %{
                       "id" => ^user2_id
                     }
                   }
                 ]
               }
             }
           } = data

    bot_ids = Enum.map(edges, &(&1["node"]["id"]))
    assert Enum.sort(bot_ids) == ids(coll_bots)
  end

  test "get collection through bot", %{
    user: user,
    collection: collection,
    coll_bots: coll_bots
  } do
    """
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
    |> run_query(user, %{"id" => hd(coll_bots).id})
    |> assert_data(%{
      "bot" => %{
        "collections" => %{
          "totalCount" => 1,
          "edges" => [
            %{
              "node" => %{
                "id" => to_string(collection.id),
                "title" => collection.title,
                "owner" => %{
                  "id" => user.id
                }
              }
            }
          ]
        }
      }
    })
  end

  test "get owned collection through current user", %{
    user: user,
    collection: collection,
    coll_bots: coll_bots
  } do
    """
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
    |> run_query(user)
    |> assert_data(%{
      "currentUser" => %{
        "collections" => %{
          "totalCount" => 1,
          "edges" => [
            %{
              "node" => %{
                "id" => to_string(collection.id),
                "title" => collection.title,
                "bots" => %{
                  "totalCount" => length(coll_bots)
                }
              }
            }
          ]
        }
      }
    })
  end

  test "get subscribed collection through user", %{
    user: user,
    user2: user2,
    collection: collection,
    coll_bots: coll_bots
  } do
    """
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
    |> run_query(user, %{"id" => user2.id})
    |> assert_data(%{
      "user" => %{
        "subscribedCollections" => %{
          "totalCount" => 1,
          "edges" => [
            %{
              "node" => %{
                "id" => to_string(collection.id),
                "title" => collection.title,
                "bots" => %{
                  "totalCount" => length(coll_bots)
                }
              }
            }
          ]
        }
      }
    })
  end

  test "create collection mutation", %{user: %{id: user_id} = user} do
    title = Lorem.sentence()

    data =
      """
      mutation ($title: String!) {
        collectionCreate (input: {title: $title}) {
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
      |> run_query(user, %{"title" => title})

    assert %{
             "collectionCreate" => %{
               "result" => %{
                 "id" => id,
                 "title" => ^title,
                 "owner" => %{
                   "id" => ^user_id
                 }
               }
             }
           } = data

    assert %Collection{title: ^title} =
             id
             |> String.to_integer()
             |> Collections.get_query(user)
             |> Repo.one!()
  end

  test "update collection mutation", %{collection: %{id: id}, user: user} do
    new_title = Lorem.sentence()

    """
    mutation ($id: AInt!, $title: String!) {
      collectionUpdate (input: {id: $id, title: $title}) {
        result {
          title
        }
      }
    }
    """
    |> run_query(user, %{"id" => to_string(id), "title" => new_title})
    |> assert_data(%{
      "collectionUpdate" => %{
        "result" => %{
          "title" => new_title
        }
      }
    })

    assert %Collection{title: ^new_title} =
             id
             |> Collections.get_query(user)
             |> Repo.one!()
  end

  test "delete collection mutation", %{collection: %{id: id}, user: user} do
    """
    mutation ($id: AInt!) {
      collectionDelete (input: {id: $id}) {
        result
      }
    }
    """
    |> run_query(user, %{"id" => to_string(id)})
    |> assert_data(%{"collectionDelete" => %{"result" => true}})

    assert nil ==
             id
             |> Collections.get_query(user)
             |> Repo.one()
  end

  test "subscribe collection mutation", %{collection: collection, user: user} do
    """
    mutation ($id: AInt!) {
      collectionSubscribe (input: {id: $id}) {
        result
      }
    }
    """
    |> run_query(user, %{"id" => to_string(collection.id)})
    |> assert_data(%{"collectionSubscribe" => %{"result" => true}})

    assert collection
           |> Collections.get_subscribers_query(user)
           |> Repo.all()
           |> Enum.any?(&(&1.id == user.id))
  end

  test "unsubscribe collection mutation", %{
    collection: collection,
    user: user
  } do
    """
    mutation ($id: AInt!) {
      collectionUnsubscribe (input: {id: $id}) {
        result
      }
    }
    """
    |> run_query(user, %{"id" => to_string(collection.id)})
    |> assert_data(%{"collectionUnsubscribe" => %{"result" => true}})

    assert collection
           |> Collections.get_subscribers_query(user)
           |> Repo.all()
           |> Enum.all?(&(&1.id != user.id))
  end

  test "add bot collection mutation", %{collection: collection, user: user} do
    bot = Factory.insert(:bot, user: user, public: true)

    """
    mutation ($id: AInt!, $botId: UUID!) {
      collectionAddBot (input: {id: $id, botId: $botId}) {
        result
      }
    }
    """
    |> run_query(user, %{"id" => to_string(collection.id), "botId" => bot.id})
    |> assert_data(%{"collectionAddBot" => %{"result" => true}})

    assert collection
           |> Collections.get_members_query(user)
           |> Repo.all()
           |> Enum.any?(&(&1.id == bot.id))
  end

  test "remove bot collection mutation", %{
    collection: collection,
    user: user,
    coll_bots: [bot | _]
  } do
    """
    mutation ($id: AInt!, $botId: UUID!) {
      collectionRemoveBot (input: {id: $id, botId: $botId}) {
        result
      }
    }
    """
    |> run_query(user, %{"id" => to_string(collection.id), "botId" => bot.id})
    |> assert_data(%{"collectionRemoveBot" => %{"result" => true}})

    assert collection
           |> Collections.get_members_query(user)
           |> Repo.all()
           |> Enum.all?(&(&1.id != bot.id))
  end

  test "collection sharing", %{
    collection: collection,
    user: user,
    user2: user2
  } do
    message = Lorem.paragraph()
    """
    mutation ($id: AInt!, $user_id: UUID!, $message: String) {
      collectionShare (input: {id: $id, user_id: $user_id, message: $message}) {
        result
      }
    }
    """
    |> run_query(user, %{
      "id" => to_string(collection.id),
      "user_id" => user2.id,
      "message" => message
    })
    |> assert_data(%{"collectionShare" => %{"result" => true}})

    %Item{stanza: stanza} = user2.id |> HomeStream.get(false) |> List.last()

    assert stanza |> xpath(~x"//message/body/text()"s) == message
    assert stanza |> xpath(~x"//collection/id/text()"s) ==
      to_string(collection.id)
    assert stanza |> xpath(~x"//collection/action/text()"s) == "share"
  end

  defp ids(items), do: Enum.sort(Enum.map(items, &(&1.id)))
end
