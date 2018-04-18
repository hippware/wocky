defmodule WockyAPI.GraphQL.UserTest do
  use WockyAPI.GraphQLCase, async: true

  alias Faker.Lorem
  alias Faker.Name
  alias Faker.String
  alias Wocky.Blocking
  alias Wocky.Repo.Factory
  alias Wocky.Repo
  alias Wocky.Repo.ID
  alias Wocky.JID
  alias Wocky.User
  alias Wocky.User.Location

  setup do
    [user, user2] = Factory.insert_list(2, :user)

    {:ok, user: user, user2: user2}
  end

  @query """
  {
    currentUser {
      id
      firstName
      avatar {
        tros_url
      }
    }
  }
  """

  test "get basic current user info", %{user: user} do
    result = run_query(@query, user)

    refute has_errors(result)

    assert result.data == %{
             "currentUser" => %{
               "id" => user.id,
               "firstName" => user.first_name,
               "avatar" => %{
                 "tros_url" => user.avatar
               }
             }
           }
  end

  @query """
  mutation ($values: UserUpdateInput!) {
    userUpdate (input: {values: $values}) {
      successful
      result {
        id
      }
    }
  }
  """

  test "update user", %{user: user} do
    new_name = Name.first_name()
    result = run_query(@query, user, %{"values" => %{"first_name" => new_name}})

    refute has_errors(result)

    assert result.data == %{
             "userUpdate" => %{
               "successful" => true,
               "result" => %{
                 "id" => user.id
               }
             }
           }

    assert Repo.get(User, user.id).first_name == new_name
  end

  @query """
  query ($id: String!) {
    user (id: $id) {
      id
      handle
    }
  }
  """

  test "get other user info", %{user: user, user2: user2} do
    result = run_query(@query, user, %{"id" => user2.id})

    refute has_errors(result)

    assert result.data == %{
             "user" => %{
               "id" => user2.id,
               "handle" => user2.handle
             }
           }
  end

  test "non-existant ID", %{user: user} do
    result = run_query(@query, user, %{"id" => ID.new()})

    assert error_count(result) == 1
    assert error_msg(result) =~ "User not found"
    assert result.data == %{"user" => nil}
  end

  test "invalid ID", %{user: user} do
    result = run_query(@query, user, %{"id" => "not_an_id"})

    assert error_count(result) == 1
    assert error_msg(result) =~ "invalid value"
    refute has_data(result)
  end

  test "blocked user", %{user: user, user2: user2} do
    Blocking.block(user2, user)

    result = run_query(@query, user, %{"id" => user2.id})

    assert error_count(result) == 1
    assert error_msg(result) =~ "User not found"
    assert result.data == %{"user" => nil}
  end

  describe "user bots" do
    setup %{user: user, user2: user2} do
      bot = Factory.insert(:bot, user: user)
      bot2 = Factory.insert(:bot, user: user2, public: true)

      {:ok, bot: bot, bot2: bot2}
    end

    @query """
    query ($id: String!) {
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

    test "get by owner", %{user: user, user2: user2, bot2: bot2} do
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

    @query """
    {
      currentUser {
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

    test "get own bots", %{user: user, bot: bot} do
      result = run_query(@query, user)

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
  end

  describe "location" do
    @query """
    mutation ($input: UserLocationUpdateInput!) {
      userLocationUpdate (input: $input) {
        successful
      }
    }
    """

    test "set location", %{user: user} do
      lat = :rand.uniform() * 89.0
      lon = :rand.uniform() * 179.0
      accuracy = :rand.uniform() * 10.0
      resource = String.base64()

      location_input = %{
        "lat" => lat,
        "lon" => lon,
        "accuracy" => accuracy,
        "resource" => resource
      }

      result = run_query(@query, user, %{"input" => location_input})

      refute has_errors(result)

      assert result.data == %{
               "userLocationUpdate" => %{
                 "successful" => true
               }
             }

      assert %Location{
               lat: ^lat,
               lon: ^lon,
               resource: ^resource,
               accuracy: ^accuracy
             } = Repo.get_by(Location, user_id: user.id)
    end

    test "invalid location", %{user: user} do
      location_input = %{
        "lat" => :rand.uniform() * 89.0,
        "lon" => :rand.uniform() * 179.0,
        "accuracy" => -1.0,
        "resource" => String.base64()
      }

      result = run_query(@query, user, %{"input" => location_input})

      refute has_errors(result)

      assert result.data == %{
               "userLocationUpdate" => %{
                 "successful" => false
               }
             }

      assert Repo.get_by(Location, user_id: user.id) == nil
    end
  end

  describe "user search" do
    setup %{user2: user2} do
      Repo.delete(user2)
      :ok
    end

    @query """
    query ($term: String!, $limit: Int) {
      users (search_term: $term, limit: $limit) {
        id
      }
    }
    """

    test "search results", %{user: user} do
      u =
        Factory.insert(
          :user,
          first_name: "Bob",
          last_name: "aaa",
          handle: "hhh"
        )

      result = run_query(@query, user, %{"term" => "b"})

      refute has_errors(result)
      assert result.data == %{"users" => [%{"id" => u.id}]}
    end

    test "search limit", %{user: user} do
      Factory.insert_list(20, :user, first_name: "aaa")

      result = run_query(@query, user, %{"term" => "a", "limit" => 10})

      assert %{"users" => results} = result.data
      assert length(results) == 10
    end
  end

  describe "home stream items" do
    @query """
    query ($first: Int) {
      currentUser {
        homeStream (first: $first) {
          totalCount
          edges {
            node {
              key
              reference_bot {
                id
              }
            }
          }
        }
      }
    }
    """

    test "get items", %{user: user} do
      bot = Factory.insert(:bot, user: user)

      items =
        Factory.insert_list(
          20,
          :home_stream_item,
          user: user,
          reference_bot: bot
        )

      result = run_query(@query, user, %{"first" => 1})

      refute has_errors(result)

      assert result.data == %{
               "currentUser" => %{
                 "homeStream" => %{
                   "totalCount" => 20,
                   "edges" => [
                     %{
                       "node" => %{
                         "key" => List.last(items).key,
                         "reference_bot" => %{"id" => bot.id}
                       }
                     }
                   ]
                 }
               }
             }
    end
  end

  describe "conversations" do
    @query """
    {
      currentUser {
        conversations (first: 1) {
          totalCount
          edges {
            node {
              other_jid
              user {
                id
                firstName
              }
            }
          }
        }
      }
    }
    """

    test "get conversations", %{user: user, user2: user2} do
      other_jid = JID.to_binary(User.to_jid(user2, Lorem.word()))
      Factory.insert(:conversation, other_jid: other_jid, user: user)

      result = run_query(@query, user)

      refute has_errors(result)

      assert result.data == %{
               "currentUser" => %{
                 "conversations" => %{
                   "totalCount" => 1,
                   "edges" => [
                     %{
                       "node" => %{
                         "other_jid" => other_jid,
                         "user" => %{
                           "id" => user2.id,
                           "firstName" => user2.first_name
                         }
                       }
                     }
                   ]
                 }
               }
             }
    end
  end
end
