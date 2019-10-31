defmodule WockyAPI.GraphQL.FriendTest do
  use WockyAPI.GraphQLCase, async: true

  alias Faker.Name
  alias Wocky.Block
  alias Wocky.Friends
  alias Wocky.Notifier.Push
  alias Wocky.Notifier.Push.Backend.Sandbox
  alias Wocky.Repo.Factory
  alias Wocky.Repo.ID

  setup do
    [user, user2] = Factory.insert_list(2, :user)

    {:ok, user: user, user2: user2}
  end

  # -------------------------------------------------------------------
  # Connections

  describe "contacts connection" do
    @query """
    query ($rel: UserContactRelationship) {
      currentUser {
        contacts (first: 1, relationship: $rel) {
          totalCount
          edges {
            relationship
            createdAt
            node {
              id
            }
          }
        }
      }
    }
    """

    test "get contacts by relationship", %{user: user, user2: user2} do
      Friends.befriend(user, user2)
      id2 = user2.id

      for rel <- [nil, "FRIEND"] do
        result = run_query(@query, user, %{"rel" => rel})

        refute has_errors(result)

        assert %{
                 "currentUser" => %{
                   "contacts" => %{
                     "edges" => [
                       %{
                         "node" => %{"id" => ^id2},
                         "relationship" => "FRIEND",
                         "createdAt" => _
                       }
                     ],
                     "totalCount" => 1
                   }
                 }
               } = result.data
      end
    end

    test "get contacts by relationship 'NONE'", %{user: user} do
      result = run_query(@query, user, %{"rel" => "NONE"})

      assert has_errors(result)

      assert [%{message: "unsupported"}] = result.errors
    end

    test "should fail for other users", ctx do
      query = """
      query  {
        user (id: "#{ctx.user2.id}") {
          contacts (first: 1, relationship: FRIEND) {
            totalCount
          }
        }
      }
      """

      result = run_query(query, ctx.user)

      assert has_errors(result)

      assert [%{message: "permission_denied"}] = result.errors
    end
  end

  describe "friends connection" do
    @query """
    query {
      currentUser {
        friends (first: 1) {
          totalCount
          edges {
            node {
              user { id }
              name
              createdAt
            }
          }
        }
      }
    }
    """

    test "get friends", %{user: user, user2: user2} do
      Friends.befriend(user, user2)

      name = Name.name()
      Friends.update_name(user, user2, name)

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
                         "name" => ^name,
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
      Friends.make_friends(user, user2, :always)
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
      Friends.make_friends(user2, user, :always)
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
      Friends.befriend(user, user2)

      {:ok, item} = Friends.update_sharing(user, user2, :always)

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
      Friends.befriend(user, user2)

      {:ok, item} = Friends.update_sharing(user, user2, :always)

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

      assert Friends.relationship(shared.user, shared.user2) == :invited
    end

    test "should make a friend from a invited_by", shared do
      Friends.make_friends(shared.user2, shared.user, :disabled)
      result = run_query(@query, shared.user, %{"userId" => shared.user2.id})
      refute has_errors(result)

      assert result.data == %{
               "friendInvite" => %{
                 "successful" => true,
                 "result" => "FRIEND",
                 "messages" => []
               }
             }

      assert Friends.relationship(shared.user, shared.user2) == :friend
    end

    test "should return an error for a blocked user", shared do
      Block.block(shared.user2, shared.user)
      result = run_query(@query, shared.user, %{"userId" => shared.user2.id})

      refute has_errors(result)

      assert %{
               "friendInvite" => %{
                 "successful" => false,
                 "result" => nil,
                 "messages" => [
                   %{
                     "field" => "inviteeId",
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
                     "field" => "inviteeId",
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
                     "field" => "inviteeId",
                     "message" => "does not exist"
                   }
                 ]
               }
             } = result.data
    end
  end

  # DEPRECATED
  describe "friendName mutation" do
    @query """
    mutation ($user_id: UUID!, $name: String!) {
      friendName (input: {user_id: $user_id, name: $name}) {
        successful
        result
        messages {
          field
          message
        }
      }
    }
    """

    test "assign a name to a friend", %{user: user, user2: user2} do
      Friends.befriend(user, user2)
      new_name = Name.name()

      result =
        run_query(@query, user, %{"user_id" => user2.id, "name" => new_name})

      refute has_errors(result)

      assert result.data == %{
               "friendName" => %{
                 "successful" => true,
                 "result" => true,
                 "messages" => []
               }
             }

      assert Friends.get_friend(user, user2).name == new_name
    end

    test "should fail when the user doesn't exist", %{user: user} do
      new_name = Name.name()

      result =
        run_query(@query, user, %{"user_id" => ID.new(), "name" => new_name})

      refute has_errors(result)

      assert result.data == %{
               "friendName" => %{
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
      new_name = Name.name()

      result =
        run_query(@query, user, %{"user_id" => user2.id, "name" => new_name})

      refute has_errors(result)

      assert result.data == %{
               "friendName" => %{
                 "successful" => false,
                 "result" => nil,
                 "messages" => [
                   %{"field" => "contactId", "message" => "must be a friend"}
                 ]
               }
             }
    end
  end

  describe "friendNameUpdate mutation" do
    @query """
    mutation ($user_id: UUID!, $name: String!) {
      friendNameUpdate (input: {user_id: $user_id, name: $name}) {
        successful
        result {
          user { id }
          name
        }
        messages {
          field
          message
        }
      }
    }
    """

    test "assign a name to a friend", %{user: user, user2: user2} do
      Friends.befriend(user, user2)
      new_name = Name.name()

      result =
        run_query(@query, user, %{"user_id" => user2.id, "name" => new_name})

      refute has_errors(result)

      assert result.data == %{
               "friendNameUpdate" => %{
                 "successful" => true,
                 "result" => %{
                   "user" => %{"id" => user2.id},
                   "name" => new_name
                 },
                 "messages" => []
               }
             }

      assert Friends.get_friend(user, user2).name == new_name
    end

    test "should fail when the user doesn't exist", %{user: user} do
      new_name = Name.name()

      result =
        run_query(@query, user, %{"user_id" => ID.new(), "name" => new_name})

      refute has_errors(result)

      assert result.data == %{
               "friendNameUpdate" => %{
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
      new_name = Name.name()

      result =
        run_query(@query, user, %{"user_id" => user2.id, "name" => new_name})

      refute has_errors(result)

      assert result.data == %{
               "friendNameUpdate" => %{
                 "successful" => false,
                 "result" => nil,
                 "messages" => [
                   %{"field" => "contactId", "message" => "must be a friend"}
                 ]
               }
             }
    end
  end

  describe "friendShareUpdate mutation" do
    @query """
    mutation ($user_id: UUID!, $share_type: FriendShareType!) {
      friendShareUpdate (input: {user_id: $user_id, share_type: $share_type}) {
        successful
        result {
          user { id }
          shareType
        }
        messages {
          field
          message
        }
      }
    }
    """

    test "change sharing level", %{user: user, user2: user2} do
      Friends.befriend(user, user2, share_type: :always)

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
                   "shareType" => "DISABLED"
                 },
                 "messages" => []
               }
             }

      assert Friends.get_friend(user, user2).share_type == :disabled
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
      Friends.befriend(shared.user, shared.user2)
      result = run_query(@query, shared.user, %{"userId" => shared.user2.id})
      refute has_errors(result)

      assert result.data == %{
               "friendDelete" => %{
                 "successful" => true,
                 "result" => true
               }
             }

      assert Friends.relationship(shared.user, shared.user2) == :none
    end

    test "should remove all relationship with an invitee", shared do
      Friends.make_friends(shared.user, shared.user2, :always)
      result = run_query(@query, shared.user, %{"userId" => shared.user2.id})
      refute has_errors(result)

      assert result.data == %{
               "friendDelete" => %{
                 "successful" => true,
                 "result" => true
               }
             }

      assert Friends.relationship(shared.user, shared.user2) == :none
    end

    test "should remove all relationship with an invited_by", shared do
      Friends.make_friends(shared.user2, shared.user, :always)
      result = run_query(@query, shared.user, %{"userId" => shared.user2.id})
      refute has_errors(result)

      assert result.data == %{
               "friendDelete" => %{
                 "successful" => true,
                 "result" => true
               }
             }

      assert Friends.relationship(shared.user, shared.user2) == :none
    end

    test "should work for a blocked user", shared do
      Block.block(shared.user2, shared.user)
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
      Friends.befriend(user, user2)

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
      {:ok, _} = Friends.update_sharing(user, user2, :always)

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
      {:ok, _} = Friends.update_sharing(user, user2, :always)

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
