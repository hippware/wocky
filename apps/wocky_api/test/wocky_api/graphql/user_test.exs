defmodule WockyAPI.GraphQL.UserTest do
  use WockyAPI.GraphQLCase, async: true

  alias Faker.Lorem
  alias Faker.Name
  alias Faker.String
  alias Wocky.Block
  alias Wocky.JID
  alias Wocky.Repo
  alias Wocky.Repo.{Factory, ID, Timestamp}
  alias Wocky.Roster
  alias Wocky.User
  alias Wocky.User.{BotEvent, Location}

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
    query ($id: UUID!) {
      user (id: $id) {
        id
        ... on CurrentUser {
          email
        }
      }
    }
    """

    test "get info via user query", %{user: user} do
      result = run_query(@query, user, %{"id" => user.id})

      refute has_errors(result)

      assert result.data == %{
               "user" => %{
                 "id" => user.id,
                 "email" => user.email
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

      result =
        run_query(@query, user, %{"values" => %{"first_name" => new_name}})

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
    mutation ($enable: Boolean!, $expire: DateTime) {
      userHide (input: {enable: $enable, expire: $expire}) {
        result
      }
    }
    """

    test "set user as permanently hidden", %{user: user} do
      result = run_query(@query, user, %{"enable" => true})

      refute has_errors(result)

      assert result.data == %{"userHide" => %{"result" => true}}

      assert User.hidden_state(Repo.get(User, user.id)) == {true, nil}
    end

    test "set user as not hidden", %{user: user} do
      result = run_query(@query, user, %{"enable" => false})

      refute has_errors(result)

      assert result.data == %{"userHide" => %{"result" => true}}

      assert User.hidden_state(Repo.get(User, user.id)) == {false, nil}
    end

    test "set user as temporarally hidden", %{user: user} do
      ts = Timestamp.shift(days: 1) |> Timestamp.to_string()
      result = run_query(@query, user, %{"enable" => true, "expire" => ts})

      refute has_errors(result)

      assert result.data == %{"userHide" => %{"result" => true}}

      assert User.hidden_state(Repo.get(User, user.id)) ==
               {true, Timestamp.from_string!(ts)}
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
      Block.block(user2, user)

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
  end

  describe "location queries" do
    setup %{user: user} do
      bot = Factory.insert(:bot, user: user)
      loc = Factory.insert(:location, user_id: user.id)
      BotEvent.insert(user, loc.resource, bot, loc, :enter)

      {:ok, loc: loc, bot: bot}
    end

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
              isFetch
            }
          }
        }
      }
    }
    """

    test "get locations", %{user: user, loc: loc} do
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
                         "accuracy" => loc.accuracy,
                         "isFetch" => false
                       }
                     }
                   ]
                 }
               }
             }
    end

    @query """
    query ($device: String!) {
      currentUser {
        locations (device: $device, first: 1) {
          totalCount
          edges {
            node {
              events (first: 1) {
                totalCount
                edges {
                  node {
                    event
                  }
                }
              }
            }
          }
        }
      }
    }
    """

    test "get events from location", %{user: user, loc: loc} do
      result = run_query(@query, user, %{"device" => loc.resource})

      refute has_errors(result)

      assert result.data == %{
               "currentUser" => %{
                 "locations" => %{
                   "totalCount" => 1,
                   "edges" => [
                     %{
                       "node" => %{
                         "events" => %{
                           "totalCount" => 1,
                           "edges" => [%{"node" => %{"event" => "ENTER"}}]
                         }
                       }
                     }
                   ]
                 }
               }
             }
    end

    @query """
    query ($device: String!) {
      currentUser {
        locationEvents (device: $device, first: 1) {
          totalCount
          edges {
            node {
              event
              location {
                lat
                lon
              }
              bot {
                id
              }
            }
          }
        }
      }
    }
    """

    test "get all location events", %{user: user, bot: bot, loc: loc} do
      result = run_query(@query, user, %{"device" => loc.resource})

      refute has_errors(result)

      assert result.data == %{
               "currentUser" => %{
                 "locationEvents" => %{
                   "totalCount" => 1,
                   "edges" => [
                     %{
                       "node" => %{
                         "event" => "ENTER",
                         "location" => %{
                           "lat" => loc.lat,
                           "lon" => loc.lon
                         },
                         "bot" => %{
                           "id" => bot.id
                         }
                       }
                     }
                   ]
                 }
               }
             }
    end
  end

  describe "location mutations" do
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
        "resource" => resource,
        "isFetch" => true
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
               accuracy: ^accuracy,
               is_fetch: true
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
              otherJid
              otherUser {
                id
                firstName
              }
              message
              owner {
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
      message = Lorem.sentence()

      Factory.insert(
        :conversation,
        other_jid: other_jid,
        user: user,
        message: message
      )

      result = run_query(@query, user)

      refute has_errors(result)

      assert result.data == %{
               "currentUser" => %{
                 "conversations" => %{
                   "totalCount" => 1,
                   "edges" => [
                     %{
                       "node" => %{
                         "otherJid" => other_jid,
                         "otherUser" => %{
                           "id" => user2.id,
                           "firstName" => user2.first_name
                         },
                         "message" => message,
                         "owner" => %{
                           "id" => user.id,
                           "firstName" => user.first_name
                         }
                       }
                     }
                   ]
                 }
               }
             }
    end
  end

  describe "hasUsedGeofence" do
    @query """
    query {
      currentUser {
        hasUsedGeofence
      }
    }
    """
    test "Should always be true", %{user: user} do
      result = run_query(@query, user)
      assert result.data == %{"currentUser" => %{"hasUsedGeofence" => true}}
    end
  end

  describe "message archive" do
    @query """
    query ($other_user: UUID) {
      currentUser {
        messages (other_user: $other_user, first: 50){
          total_count
          edges {
            node {
              other_user {
                id
              }
              message
            }
          }
        }
      }
    }
    """
    setup shared do
      user3 = Factory.insert(:user)

      messages2 =
        Enum.map(1..5, fn _ ->
          Factory.insert_message(shared.user, shared.user2)
        end)

      messages3 =
        Enum.map(1..3, fn _ -> Factory.insert_message(shared.user, user3) end)

      {:ok, user3: user3, messages2: messages2, messages3: messages3}
    end

    test "should successfully retrieve all messages", %{
      user: user,
      user2: user2,
      user3: user3
    } do
      result = run_query(@query, user)

      refute has_errors(result)

      assert %{
               "currentUser" => %{
                 "messages" => %{
                   "total_count" => 8,
                   "edges" => edges
                 }
               }
             } = result.data

      assert length(edges) == 8

      assert edges
             |> Enum.map(& &1["node"]["other_user"]["id"])
             |> Enum.uniq()
             |> Enum.sort() == Enum.sort([user2.id, user3.id])
    end

    test "should retrieve all messages for a given user", %{
      user: user,
      user2: user2
    } do
      result = run_query(@query, user, %{"other_user" => user2.id})

      refute has_errors(result)

      assert %{
               "currentUser" => %{
                 "messages" => %{
                   "total_count" => 5,
                   "edges" => edges
                 }
               }
             } = result.data

      assert length(edges) == 5

      assert edges |> Enum.map(& &1["node"]["other_user"]["id"]) |> Enum.uniq() ==
               Enum.sort([user2.id])
    end

    test "should return an error for a non-existant user", %{user: user} do
      result = run_query(@query, user, %{"other_user" => ID.new()})

      assert has_errors(result)

      assert error_msg(result) =~ "User not found"
    end
  end

  describe "invitation codes" do
    @query """
    mutation {
      userInviteMakeCode {
        successful
        result
      }
    }
    """

    test "get invitation code", %{user: user} do
      result = run_query(@query, user)

      refute has_errors(result)

      assert %{
               "userInviteMakeCode" => %{
                 "successful" => true,
                 "result" => code
               }
             } = result.data

      assert is_binary(code)
      assert byte_size(code) > 1
    end

    @query """
    mutation ($code: String!) {
      userInviteRedeemCode(input: {code: $code}) {
        result
      }
    }
    """

    test "redeem invitation code", %{user: user} do
      inviter = Factory.insert(:user)
      code = User.make_invite_code(inviter)

      result = run_query(@query, user, %{"code" => code})
      refute has_errors(result)
      assert result.data == %{"userInviteRedeemCode" => %{"result" => true}}
    end
  end

  describe "delete user mutation" do
    test "Should be false with no related bots", %{user: user} do
      query = "mutation { userDelete { result } }"
      result = run_query(query, user)
      refute has_errors(result)
      assert result.data == %{"userDelete" => %{"result" => true}}
      assert User.get_user(user.id) == nil
    end
  end

  describe "follow mutation" do
    @query """
    mutation ($userId: UUID!) {
      follow(input: {userId: $userId}) {
        successful
        result {
          relationship
          user { id }
        }
      }
    }
    """
    test "should make a follower from a non-relationship", shared do
      result = run_query(@query, shared.user, %{"userId" => shared.user2.id})
      refute has_errors(result)

      assert result.data == %{
               "follow" => %{
                 "successful" => true,
                 "result" => %{
                   "relationship" => "FOLLOWER",
                   "user" => %{"id" => shared.user2.id}
                 }
               }
             }

      assert Roster.relationship(shared.user.id, shared.user2.id) == :follower
    end

    test "should make a friend from a follower", shared do
      Roster.follow(shared.user2.id, shared.user.id)
      result = run_query(@query, shared.user, %{"userId" => shared.user2.id})
      refute has_errors(result)

      assert result.data == %{
               "follow" => %{
                 "successful" => true,
                 "result" => %{
                   "relationship" => "FRIEND",
                   "user" => %{"id" => shared.user2.id}
                 }
               }
             }

      assert Roster.relationship(shared.user.id, shared.user2.id) == :friend
    end

    test "should return an error for a blocked user", shared do
      Block.block(shared.user2, shared.user)
      result = run_query(@query, shared.user, %{"userId" => shared.user2.id})
      assert has_errors(result)
      assert error_msg(result) =~ "Invalid user"
    end

    test "should return an error if you try to follow yourself", shared do
      result = run_query(@query, shared.user, %{"userId" => shared.user.id})
      assert has_errors(result)
      assert error_msg(result) =~ "Invalid user"
    end

    test "should return an error for a non-existant", shared do
      result = run_query(@query, shared.user, %{"userId" => ID.new()})
      assert has_errors(result)
      assert error_msg(result) =~ "Invalid user"
    end
  end

  describe "unfollow mutation" do
    @query """
    mutation ($userId: UUID!) {
      unfollow(input: {userId: $userId}) {
        successful
        result {
          relationship
          user { id }
        }
      }
    }
    """
    test "should make a follower from a friendship", shared do
      Roster.befriend(shared.user.id, shared.user2.id)
      result = run_query(@query, shared.user, %{"userId" => shared.user2.id})
      refute has_errors(result)

      assert result.data == %{
               "unfollow" => %{
                 "successful" => true,
                 "result" => %{
                   "relationship" => "FOLLOWING",
                   "user" => %{"id" => shared.user2.id}
                 }
               }
             }

      assert Roster.relationship(shared.user.id, shared.user2.id) == :followee
    end

    test "should make a non-contact from a followee", shared do
      Roster.follow(shared.user.id, shared.user2.id)
      result = run_query(@query, shared.user, %{"userId" => shared.user2.id})
      refute has_errors(result)

      assert result.data == %{
               "unfollow" => %{
                 "successful" => true,
                 "result" => %{
                   "relationship" => "NONE",
                   "user" => %{"id" => shared.user2.id}
                 }
               }
             }

      assert Roster.relationship(shared.user.id, shared.user2.id) == :none
    end

    test "should return an error for a blocked user", shared do
      Block.block(shared.user2, shared.user)
      result = run_query(@query, shared.user, %{"userId" => shared.user2.id})
      assert has_errors(result)
      assert error_msg(result) =~ "Invalid user"
    end

    test "should return an error if you try to unfollow yourself", shared do
      result = run_query(@query, shared.user, %{"userId" => shared.user.id})
      assert has_errors(result)
      assert error_msg(result) =~ "Invalid user"
    end

    test "should return an error for a non-existant", shared do
      result = run_query(@query, shared.user, %{"userId" => ID.new()})
      assert has_errors(result)
      assert error_msg(result) =~ "Invalid user"
    end
  end
end
