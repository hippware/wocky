defmodule WockyAPI.GraphQL.ContactTest do
  use WockyAPI.GraphQLCase, async: true

  alias Wocky.Contacts
  alias Wocky.Notifier.Push
  alias Wocky.Notifier.Push.Backend.Sandbox
  alias Wocky.Repo.ID

  setup do
    [user, user2] = Factory.insert_list(2, :user)

    {:ok, user: user, user2: user2}
  end

  # -------------------------------------------------------------------
  # Connections

  describe "friends connection" do
    @query """
    query {
      currentUser {
        friends (first: 1) {
          totalCount
          edges {
            node {
              user { id }
              createdAt
            }
          }
        }
      }
    }
    """

    test "should show friends", %{user: user, user2: user2} do
      Contacts.befriend(user, user2)

      id2 = user2.id

      result = run_query(@query, user)

      refute has_errors(result)

      assert %{
               "currentUser" => %{
                 "friends" => %{
                   "edges" => [
                     %{
                       "node" => %{
                         "user" => %{"id" => ^id2},
                         "createdAt" => _
                       }
                     }
                   ],
                   "totalCount" => 1
                 }
               }
             } = result.data
    end

    test "should not show invitees", %{user: user, user2: user2} do
      {:ok, :invited} = Contacts.make_friends(user, user2, :always)

      result = run_query(@query, user)

      refute has_errors(result)

      assert %{
               "currentUser" => %{
                 "friends" => %{
                   "edges" => [],
                   "totalCount" => 0
                 }
               }
             } = result.data
    end
  end

  describe "sentInvitations connection" do
    @query """
    query {
      currentUser {
        sentInvitations (first: 1) {
          totalCount
          edges {
            node {
              sender { id }
              recipient { id }
              shareType
              createdAt
            }
          }
        }
      }
    }
    """

    test "get sent_invitations", %{user: user, user2: user2} do
      Contacts.make_friends(user, user2, :always)
      id = user.id
      id2 = user2.id

      result = run_query(@query, user)

      refute has_errors(result)

      assert %{
               "currentUser" => %{
                 "sentInvitations" => %{
                   "edges" => [
                     %{
                       "node" => %{
                         "sender" => %{"id" => ^id},
                         "recipient" => %{"id" => ^id2},
                         "shareType" => "ALWAYS",
                         "createdAt" => _
                       }
                     }
                   ],
                   "totalCount" => 1
                 }
               }
             } = result.data
    end
  end

  describe "receivedInvitations connection" do
    @query """
    query {
      currentUser {
        receivedInvitations (first: 1) {
          totalCount
          edges {
            node {
              sender { id }
              recipient { id }
              shareType
              createdAt
            }
          }
        }
      }
    }
    """

    test "get received_invitations", %{user: user, user2: user2} do
      Contacts.make_friends(user2, user, :always)
      id = user.id
      id2 = user2.id

      result = run_query(@query, user)

      refute has_errors(result)

      assert %{
               "currentUser" => %{
                 "receivedInvitations" => %{
                   "edges" => [
                     %{
                       "node" => %{
                         "sender" => %{"id" => ^id2},
                         "recipient" => %{"id" => ^id},
                         "shareType" => "ALWAYS",
                         "createdAt" => _
                       }
                     }
                   ],
                   "totalCount" => 1
                 }
               }
             } = result.data
    end
  end

  describe "locationShares connection" do
    @query """
    query {
      currentUser {
        locationShares (first: 1) {
          totalCount
          edges {
            node {
              id
              user { id }
              sharedWith { id }
              shareType
              expiresAt
            }
          }
        }
      }
    }
    """

    test "get user's sharing sessions", %{user: user, user2: user2} do
      Contacts.befriend(user, user2)

      {:ok, item} = Contacts.update_sharing(user, user2, :always)

      sharer = user.id
      shared_with = user2.id
      id = to_string(item.share_id)

      result = run_query(@query, user, %{})

      refute has_errors(result)

      assert %{
               "currentUser" => %{
                 "locationShares" => %{
                   "edges" => [
                     %{
                       "node" => %{
                         "id" => ^id,
                         "user" => %{"id" => ^sharer},
                         "sharedWith" => %{"id" => ^shared_with},
                         "shareType" => "ALWAYS"
                       }
                     }
                   ],
                   "totalCount" => 1
                 }
               }
             } = result.data
    end
  end

  describe "locationSharers connection" do
    @query """
    query {
      currentUser {
        locationSharers (first: 1) {
          totalCount
          edges {
            node {
              id
              user { id }
              sharedWith { id }
              shareType
              expiresAt
            }
          }
        }
      }
    }
    """

    test "get sharing sessions with user", %{user: user, user2: user2} do
      Contacts.befriend(user, user2)

      {:ok, item} = Contacts.update_sharing(user, user2, :always)

      sharer = user.id
      shared_with = user2.id
      id = to_string(item.share_id)

      result = run_query(@query, user2, %{})

      refute has_errors(result)

      assert %{
               "currentUser" => %{
                 "locationSharers" => %{
                   "edges" => [
                     %{
                       "node" => %{
                         "id" => ^id,
                         "user" => %{"id" => ^sharer},
                         "sharedWith" => %{"id" => ^shared_with},
                         "shareType" => "ALWAYS"
                       }
                     }
                   ],
                   "totalCount" => 1
                 }
               }
             } = result.data
    end
  end

  # -------------------------------------------------------------------
  # Mutations

  describe "friendInvite mutation" do
    @query """
    mutation ($userId: UUID!) {
      friendInvite(input: {userId: $userId}) {
        successful
        result
        messages {
          field
          message
        }
      }
    }
    """

    test "should make a invited user from a non-relationship", shared do
      result = run_query(@query, shared.user, %{"userId" => shared.user2.id})
      refute has_errors(result)

      assert result.data == %{
               "friendInvite" => %{
                 "successful" => true,
                 "result" => "INVITED",
                 "messages" => []
               }
             }

      assert Contacts.relationship(shared.user, shared.user2) == :invited
    end

    test "should make a friend from a invited_by", shared do
      Contacts.make_friends(shared.user2, shared.user, :disabled)
      result = run_query(@query, shared.user, %{"userId" => shared.user2.id})
      refute has_errors(result)

      assert result.data == %{
               "friendInvite" => %{
                 "successful" => true,
                 "result" => "FRIEND",
                 "messages" => []
               }
             }

      assert Contacts.relationship(shared.user, shared.user2) == :friend
    end

    test "should return an error for a blocked user", shared do
      Contacts.block(shared.user2, shared.user)
      result = run_query(@query, shared.user, %{"userId" => shared.user2.id})

      refute has_errors(result)

      assert %{
               "friendInvite" => %{
                 "successful" => false,
                 "result" => nil,
                 "messages" => [
                   %{
                     "field" => "contactId",
                     "message" => "blocked"
                   }
                 ]
               }
             } = result.data
    end

    test "should return an error if you try to follow yourself", shared do
      result = run_query(@query, shared.user, %{"userId" => shared.user.id})

      refute has_errors(result)

      assert %{
               "friendInvite" => %{
                 "successful" => false,
                 "result" => nil,
                 "messages" => [
                   %{
                     "field" => "contactId",
                     "message" => "self"
                   }
                 ]
               }
             } = result.data
    end

    test "should return an error for a non-existant", shared do
      result = run_query(@query, shared.user, %{"userId" => ID.new()})

      refute has_errors(result)

      assert %{
               "friendInvite" => %{
                 "successful" => false,
                 "result" => nil,
                 "messages" => [
                   %{
                     "field" => "contact",
                     "message" => "does not exist"
                   }
                 ]
               }
             } = result.data
    end
  end

  describe "friendShareUpdate mutation" do
    @query """
    mutation ($user_id: UUID!, $share_type: FriendShareType!, $share_config: FriendShareConfigInput) {
      friendShareUpdate (input: {user_id: $user_id, share_type: $share_type, share_config: $share_config}) {
        successful
        result {
          user { id }
          shareType
          shareConfig {
            nearbyDistance
            nearbyCooldown
          }
        }
        messages {
          field
          message
        }
      }
    }
    """

    test "change sharing level", %{user: user, user2: user2} do
      Contacts.befriend(user, user2, :always)

      result =
        run_query(@query, user, %{
          "user_id" => user2.id,
          "share_type" => "DISABLED"
        })

      refute has_errors(result)

      assert result.data == %{
               "friendShareUpdate" => %{
                 "successful" => true,
                 "result" => %{
                   "user" => %{"id" => user2.id},
                   "shareType" => "DISABLED",
                   "shareConfig" => %{
                     "nearbyDistance" => 2000,
                     "nearbyCooldown" => :timer.hours(2)
                   }
                 },
                 "messages" => []
               }
             }

      assert Contacts.share_type(user, user2) == :disabled
    end

    test "change nearby range and cooldown", %{user: user, user2: user2} do
      Contacts.befriend(user, user2, :always)

      result =
        run_query(@query, user, %{
          "user_id" => user2.id,
          "share_type" => "NEARBY",
          "share_config" => %{
            "nearby_distance" => 1000,
            "nearby_cooldown" => 5
          }
        })

      refute has_errors(result)

      assert result.data == %{
               "friendShareUpdate" => %{
                 "successful" => true,
                 "result" => %{
                   "user" => %{"id" => user2.id},
                   "shareType" => "NEARBY",
                   "shareConfig" => %{
                     "nearbyDistance" => 1000,
                     "nearbyCooldown" => 5
                   }
                 },
                 "messages" => []
               }
             }

      assert {:friend, relationship, _} = Contacts.get_relationship(user, user2)

      assert relationship.share_type == :nearby
      assert relationship.nearby_distance == 1000
      assert relationship.nearby_cooldown == 5
    end

    test "Should fail when the nearby range is less than the minimum", %{
      user: user,
      user2: user2
    } do
      Contacts.befriend(user, user2, :always)

      result =
        run_query(@query, user, %{
          "user_id" => user2.id,
          "share_type" => "NEARBY",
          "share_config" => %{
            "nearby_distance" => 5
          }
        })

      assert error_count(result) == 1
      assert error_msg(result) =~ "nearbyDistance must be at least"
    end

    test "Should fail when the nearby cooldown is less than 0", %{
      user: user,
      user2: user2
    } do
      Contacts.befriend(user, user2, :always)

      result =
        run_query(@query, user, %{
          "user_id" => user2.id,
          "share_type" => "NEARBY",
          "share_config" => %{
            "nearby_cooldown" => -1
          }
        })

      assert error_count(result) == 1
      assert error_msg(result) == "nearbyCooldown must be at least 0"
    end

    test "should fail when the user doesn't exist", %{user: user} do
      result =
        run_query(@query, user, %{
          "user_id" => ID.new(),
          "share_type" => "ALWAYS"
        })

      refute has_errors(result)

      assert result.data == %{
               "friendShareUpdate" => %{
                 "successful" => false,
                 "result" => nil,
                 "messages" => [
                   %{"field" => "contactId", "message" => "must be a friend"}
                 ]
               }
             }
    end

    test "should fail when the user is not a friend", %{
      user: user,
      user2: user2
    } do
      result =
        run_query(@query, user, %{
          "user_id" => user2.id,
          "share_type" => "ALWAYS"
        })

      refute has_errors(result)

      assert result.data == %{
               "friendShareUpdate" => %{
                 "successful" => false,
                 "result" => nil,
                 "messages" => [
                   %{"field" => "contactId", "message" => "must be a friend"}
                 ]
               }
             }
    end
  end

  describe "friendDelete mutation" do
    @query """
    mutation ($userId: UUID!) {
      friendDelete(input: {userId: $userId}) {
        successful
        result
      }
    }
    """

    test "should remove all relationship with a friend", shared do
      Contacts.befriend(shared.user, shared.user2)
      result = run_query(@query, shared.user, %{"userId" => shared.user2.id})
      refute has_errors(result)

      assert result.data == %{
               "friendDelete" => %{
                 "successful" => true,
                 "result" => true
               }
             }

      assert Contacts.relationship(shared.user, shared.user2) == :none
    end

    test "should remove all relationship with an invitee", shared do
      Contacts.make_friends(shared.user, shared.user2, :always)
      result = run_query(@query, shared.user, %{"userId" => shared.user2.id})
      refute has_errors(result)

      assert result.data == %{
               "friendDelete" => %{
                 "successful" => true,
                 "result" => true
               }
             }

      assert Contacts.relationship(shared.user, shared.user2) == :none
    end

    test "should remove all relationship with an invited_by", shared do
      Contacts.make_friends(shared.user2, shared.user, :always)
      result = run_query(@query, shared.user, %{"userId" => shared.user2.id})
      refute has_errors(result)

      assert result.data == %{
               "friendDelete" => %{
                 "successful" => true,
                 "result" => true
               }
             }

      assert Contacts.relationship(shared.user, shared.user2) == :none
    end

    test "should work for a blocked user", shared do
      Contacts.block(shared.user2, shared.user)
      result = run_query(@query, shared.user, %{"userId" => shared.user2.id})
      refute has_errors(result)
    end

    test "should work when you try to unfriend yourself", shared do
      result = run_query(@query, shared.user, %{"userId" => shared.user.id})
      refute has_errors(result)
    end

    test "should work for a non-existant friend", shared do
      result = run_query(@query, shared.user, %{"userId" => ID.new()})
      refute has_errors(result)
    end
  end

  # DEPRECATED
  describe "live location sharing mutations" do
    setup %{user: user, user2: user2} do
      Contacts.befriend(user, user2)

      :ok
    end

    @query """
    mutation ($input: UserLocationLiveShareInput!) {
      userLocationLiveShare (input: $input) {
        successful
        messages {
          field
          message
        }
        result {
          user { id }
          sharedWith { id }
          expiresAt
        }
      }
    }
    """

    test "start sharing location", %{user: user, user2: user2} do
      sharer = user.id
      shared_with = user2.id
      expiry = sharing_expiry()

      result =
        run_query(@query, user, %{
          "input" => %{
            "sharedWithId" => shared_with,
            "expiresAt" => expiry
          }
        })

      refute has_errors(result)

      assert %{
               "userLocationLiveShare" => %{
                 "successful" => true,
                 "result" => %{
                   "user" => %{"id" => ^sharer},
                   "sharedWith" => %{"id" => ^shared_with},
                   "expiresAt" => ^expiry
                 }
               }
             } = result.data
    end

    test "start sharing location with a stranger", %{user: user} do
      shared_with = Factory.insert(:user).id
      expiry = sharing_expiry()

      result =
        run_query(@query, user, %{
          "input" => %{
            "sharedWithId" => shared_with,
            "expiresAt" => expiry
          }
        })

      refute has_errors(result)

      assert %{
               "userLocationLiveShare" => %{
                 "successful" => false,
                 "result" => nil,
                 "messages" => [
                   %{
                     "field" => "contactId",
                     "message" => "must be a friend"
                   }
                 ]
               }
             } = result.data
    end

    @query """
    mutation ($input: UserLocationCancelShareInput!) {
      userLocationCancelShare (input: $input) {
        successful
        result
      }
    }
    """

    test "stop sharing location", %{user: user, user2: user2} do
      {:ok, _} = Contacts.update_sharing(user, user2, :always)

      result =
        run_query(@query, user, %{
          "input" => %{
            "sharedWithId" => user2.id
          }
        })

      refute has_errors(result)

      assert %{
               "userLocationCancelShare" => %{
                 "successful" => true,
                 "result" => true
               }
             } = result.data
    end

    @query """
    mutation {
      userLocationCancelAllShares {
        successful
        result
      }
    }
    """

    test "stop all location sharing", %{user: user, user2: user2} do
      {:ok, _} = Contacts.update_sharing(user, user2, :always)

      result = run_query(@query, user, %{})

      refute has_errors(result)

      assert %{
               "userLocationCancelAllShares" => %{
                 "successful" => true,
                 "result" => true
               }
             } = result.data
    end

    @query """
    mutation ($userId: String!) {
      userLocationRequestTrigger(input: {userId: $userId}) {
        result
      }
    }
    """

    test "trigger location share request", %{user: user} do
      Sandbox.clear_notifications()

      Push.enable(user, "testing", Faker.Code.isbn13())
      result = run_query(@query, user, %{"userId" => user.id})

      refute has_errors(result)

      assert %{
               "userLocationRequestTrigger" => %{
                 "result" => true
               }
             } == result.data

      notifications = Sandbox.wait_notifications(count: 1, timeout: 5000)
      assert Enum.count(notifications) == 1
    end
  end
end
