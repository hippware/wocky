defmodule WockyAPI.GraphQL.UserTest do
  use WockyAPI.GraphQLCase, async: true

  alias Faker.Lorem
  alias Faker.Name
  alias Faker.String
  alias Wocky.Blocking
  alias Wocky.Repo
  alias Wocky.Repo.Factory
  alias Wocky.Repo.ID
  alias Wocky.Roster
  alias Wocky.JID
  alias Wocky.User
  alias Wocky.User.Location

  setup do
    [user, user2] = Factory.insert_list(2, :user)

    {:ok, user: user, user2: user2}
  end

  describe "current user" do
    @query """
    {
      currentUser {
        id
        firstName
        email
        avatar {
          tros_url
        }
      }
    }
    """

    test "get user info", %{user: user} do
      result = run_query(@query, user)

      refute has_errors(result)

      assert result.data == %{
               "currentUser" => %{
                 "id" => user.id,
                 "firstName" => user.first_name,
                 "email" => user.email,
                 "avatar" => %{
                   "tros_url" => user.avatar
                 }
               }
             }
    end

    @query """
    {
      currentUser {
        id
      }
    }

    """

    test "get user info anonymously" do
      result = run_query(@query)

      assert error_count(result) == 1
      assert error_msg(result) =~ "requires an authenticated user"
      assert result.data == %{"currentUser" => nil}
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

    test "update user info", %{user: user} do
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
  end

  describe "other user" do
    @query """
    query ($id: String!) {
      user (id: $id) {
        id
        handle
      }
    }
    """

    test "get user info", %{user: user, user2: user2} do
      result = run_query(@query, user, %{"id" => user2.id})

      refute has_errors(result)

      assert result.data == %{
               "user" => %{
                 "id" => user2.id,
                 "handle" => user2.handle
               }
             }
    end

    test "get user info with non-existant ID", %{user: user} do
      result = run_query(@query, user, %{"id" => ID.new()})

      assert error_count(result) == 1
      assert error_msg(result) =~ "User not found"
      assert result.data == %{"user" => nil}
    end

    test "get user info with invalid ID", %{user: user} do
      result = run_query(@query, user, %{"id" => "not_an_id"})

      assert error_count(result) == 1
      assert error_msg(result) =~ "invalid value"
      refute has_data(result)
    end

    test "get user info for blocked user", %{user: user, user2: user2} do
      Blocking.block(user2, user)

      result = run_query(@query, user, %{"id" => user2.id})

      assert error_count(result) == 1
      assert error_msg(result) =~ "User not found"
      assert result.data == %{"user" => nil}
    end

    test "get user info anonymously", %{user2: user2} do
      result = run_query(@query, nil, %{"id" => user2.id})

      refute has_errors(result)

      assert result.data == %{
               "user" => %{
                 "id" => user2.id,
                 "handle" => user2.handle
               }
             }
    end

    test "get user info anonymously with non-existant ID" do
      result = run_query(@query, nil, %{"id" => ID.new()})

      assert error_count(result) == 1
      assert error_msg(result) =~ "User not found"
      assert result.data == %{"user" => nil}
    end

    @query """
    query ($id: String!) {
      user (id: $id) {
        id
        email
        phone_number
        external_id
      }
    }
    """

    test "get protected field on other user", %{user: user, user2: user2} do
      result = run_query(@query, user, %{"id" => user2.id})

      assert error_count(result) == 3
      assert error_msg(result) =~ "authenticated user"

      assert result.data == %{
               "user" => %{
                 "id" => user2.id,
                 "email" => nil,
                 "phone_number" => nil,
                 "external_id" => nil
               }
             }
    end
  end

  describe "location" do
    @query """
    query ($device: String!) {
      currentUser {
        locations (device: $device, first: 1) {
          totalCount
          edges {
            node {
              lat
              lon
              accuracy
            }
          }
        }
      }
    }
    """

    test "get locations", %{user: user} do
      loc = Factory.insert(:location, user_id: user.id, resource: String.base64())
      result = run_query(@query, user, %{"device" => loc.resource})

      refute has_errors(result)

      assert result.data == %{
               "currentUser" => %{
                 "locations" => %{
                   "totalCount" => 1,
                   "edges" => [
                     %{
                       "node" => %{
                         "lon" => loc.lon,
                         "lat" => loc.lat,
                         "accuracy" => loc.accuracy
                       }
                     }
                   ]
                 }
               }
             }
    end

    @query """
    query ($device: String!, $id: UUID!) {
      user (id: $id) {
        locations (device: $device, first: 1) {
          totalCount
          edges {
            node {
              lat
              lon
              accuracy
            }
          }
        }
      }
    }
    """

    test "get locations for other user", %{user: user, user2: user2} do
      loc = Factory.insert(:location, user_id: user2.id, resource: String.base64())

      result =
        run_query(@query, user, %{
          "id" => user2.id,
          "device" => loc.resource
        })

      assert error_count(result) == 1
      assert error_msg(result) =~ "the authenticated user"
      assert result.data == %{"user" => %{"locations" => nil}}
    end

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

  describe "contacts" do
    setup %{user: user, user2: user2} do
      Roster.befriend(user.id, user2.id)
      :ok
    end

    @query """
    query ($rel: UserContactRelationship) {
      currentUser {
        contacts (first: 1, relationship: $rel) {
          totalCount
          edges {
            relationship
            node {
              id
            }
          }
        }
      }
    }
    """

    test "get contacts by relationship", %{user: user, user2: user2} do
      for rel <- [nil, "FRIEND", "FOLLOWER", "FOLLOWING"] do
        result = run_query(@query, user, %{"rel" => rel})

        refute has_errors(result)

        assert result.data == %{
                 "currentUser" => %{
                   "contacts" => %{
                     "edges" => [
                       %{
                         "node" => %{"id" => user2.id},
                         "relationship" => "FRIEND"
                       }
                     ],
                     "totalCount" => 1
                   }
                 }
               }
      end
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
