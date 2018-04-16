defmodule WockyAPI.GraphQL.PublicTest do
  use WockyAPI.ConnCase, async: true

  alias Wocky.Bot.Subscription
  alias Wocky.Repo.Factory

  setup do
    [user, user2] = Factory.insert_list(2, :user)
    bot = Factory.insert(:bot, user: user, public: true)
    bot2 = Factory.insert(:bot, user: user2)
    item = Factory.insert(:item, bot: bot, user: user2)
    Subscription.put(user, bot)
    Subscription.put(user2, bot)

    {:ok,
      user: user,
      user2: user2,
      bot: bot,
      bot2: bot2,
      item: item,
      conn: build_conn()}
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
      items (first: 10) {
        totalCount
        edges {
          node {
            id
            owner {
              handle
            }
          }
        }
      }
    }
  }
  """
  test "get public bot, subscribers, items, and their public owned bots",
  %{conn: conn,
    bot: %{id: bot_id} = bot,
    user: %{id: user_id},
    user2: %{id: user2_id, handle: user2_handle},
    item: %{id: item_id}} do
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
                    "id" => user2_id,
                    "bots" => %{
                      "totalCount" => 0,
                      "edges" => []
                    }
                  }
                },
                %{
                  "node" => %{
                    "id" => user_id,
                    "bots" => %{
                      "totalCount" => 1,
                      "edges" => [%{
                        "node" => %{
                          "id" => bot_id
                        }
                      }]
                    }
                  }
                }]
            },
            "items" => %{
              "totalCount" => 1,
              "edges" => [%{
                "node" => %{
                  "id" => item_id,
                  "owner" => %{
                    "handle" => user2_handle
                  }
                }
              }]
            }
          }
        }
      }
    end

  # GraphiQL schema query:
  @query """
  query IntrospectionQuery {
      __schema {
        queryType { name }
        mutationType { name }
        subscriptionType { name }
        types {
          ...FullType
        }
        directives {
          name
          description
          locations
          args {
            ...InputValue
          }
        }
      }
    }

    fragment FullType on __Type {
      kind
      name
      description
      fields(includeDeprecated: true) {
        name
        description
        args {
          ...InputValue
        }
        type {
          ...TypeRef
        }
        isDeprecated
        deprecationReason
      }
      inputFields {
        ...InputValue
      }
      interfaces {
        ...TypeRef
      }
      enumValues(includeDeprecated: true) {
        name
        description
        isDeprecated
        deprecationReason
      }
      possibleTypes {
        ...TypeRef
      }
    }

    fragment InputValue on __InputValue {
      name
      description
      type { ...TypeRef }
      defaultValue
    }

    fragment TypeRef on __Type {
      kind
      name
      ofType {
        kind
        name
        ofType {
          kind
          name
          ofType {
            kind
            name
            ofType {
              kind
              name
              ofType {
                kind
                name
                ofType {
                  kind
                  name
                  ofType {
                    kind
                    name
                  }
                }
              }
            }
          }
        }
      }
    }
  """
  test "schema visibility", %{conn: conn} do
    result = assert post_conn(conn, @query, 200)
    assert result["data"]["__schema"] != nil
    refute Map.has_key?(result, "errors")
  end
end
