defmodule WockyAPI.GraphQL.PublicTest do
  use WockyAPI.GraphQLCase, async: true

  alias Wocky.Account
  alias Wocky.Relation
  alias Wocky.Repo.Factory
  alias WockyAPI.Factory, as: APIFactory

  setup do
    users = [user, user2] = Factory.insert_list(2, :user)

    Enum.map(users, fn u ->
      image = Factory.insert(:tros_metadata, user: u)
      Account.update(u, %{avatar: APIFactory.image_url(image)})
    end)

    image = Factory.insert(:tros_metadata, user: user)

    bot =
      Factory.insert(
        :bot,
        user: user,
        image_url: APIFactory.image_url(image)
      )

    bot2 = Factory.insert(:bot, user: user2)
    item = Factory.insert(:item, bot: bot, user: user2)
    Relation.subscribe(user, bot)
    Relation.subscribe(user2, bot)

    {:ok, user: user, user2: user2, bot: bot, bot2: bot2, item: item}
  end

  @query """
  query IntrospectionQuery {
      __schema {
        queryType { name }
      }
  }
  """
  test "simple schema" do
    Application.put_env(:wocky, :go, true)
    result = run_query(@query)

    refute has_errors(result)
    assert result.data["__schema"] != nil
  end

  @query """
  query ($id: String) {
    bot (id: $id) {
      id
    }
  }
  """
  test "simple bot", %{bot: %{id: bot_id}} do
    Application.put_env(:wocky, :go, true)
    result = run_query(@query, nil, %{"id" => bot_id})

    assert has_errors(result)
    assert error_msg(result) =~ "requires an authenticated user"
  end

  @query """
  query ($id: String) {
    user (id: $id) {
      id
    }
  }
  """
  test "simple user", %{user: %{id: user_id}} do
    Application.put_env(:wocky, :go, true)
    result = run_query(@query, nil, %{"id" => user_id})

    assert has_errors(result)
    assert error_msg(result) =~ "requires an authenticated user"
  end

  @query """
  query ($id: String) {
    bot (id: $id) {
      id
      media {
        fullUrl
        thumbnailUrl
      }
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
              media {
                fullUrl
                thumbnailUrl
              }
            }
          }
        }
      }
    }
  }
  """
  test "get public bot, subscribers, items, and their public owned bots", %{
    bot: %{id: bot_id}
  } do
    result = run_query(@query, nil, %{"id" => bot_id})

    assert has_errors(result)
    assert error_msg(result) =~ "requires an authenticated user"
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
  test "schema visibility" do
    result = run_query(@query)

    refute has_errors(result)
    assert result.data["__schema"] != nil
  end
end
