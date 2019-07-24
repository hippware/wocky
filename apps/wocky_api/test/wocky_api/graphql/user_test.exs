defmodule WockyAPI.GraphQL.UserTest do
  use WockyAPI.GraphQLCase, async: true

  alias Faker.Lorem
  alias Faker.Name
  alias Wocky.Account
  alias Wocky.Account.User
  alias Wocky.Block
  alias Wocky.Location
  alias Wocky.Notifier.Push
  alias Wocky.Notifier.Push.Token
  alias Wocky.Notifier.Push.Backend.Sandbox
  alias Wocky.Repo
  alias Wocky.Repo.Factory
  alias Wocky.Repo.ID
  alias Wocky.Roster

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
        media {
          tros_url
        }
        updated_at
        hidden {
          enabled
          expires
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
                 "firstName" => Account.first_name(user),
                 "email" => user.email,
                 "media" => %{
                   "tros_url" => user.image_url
                 },
                 "updated_at" => DateTime.to_iso8601(user.updated_at),
                 "hidden" => %{
                   "enabled" => false,
                   "expires" =>
                     0 |> DateTime.from_unix!() |> DateTime.to_iso8601()
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
      client_data = Lorem.paragraph()

      result =
        run_query(@query, user, %{
          "values" => %{"first_name" => new_name, "client_data" => client_data}
        })

      refute has_errors(result)

      assert result.data == %{
               "userUpdate" => %{
                 "successful" => true,
                 "result" => %{
                   "id" => user.id
                 }
               }
             }

      assert User
             |> Repo.get(user.id)
             |> Account.first_name() == new_name

      assert Repo.get(User, user.id).client_data == client_data
    end

    test "update user info without name", %{user: user} do
      handle = Factory.handle()

      result =
        run_query(@query, user, %{
          "values" => %{"handle" => handle}
        })

      refute has_errors(result)

      assert result.data == %{
               "userUpdate" => %{
                 "successful" => true,
                 "result" => %{
                   "id" => user.id
                 }
               }
             }

      assert Repo.get(User, user.id).handle == handle
    end

    test "Set single name from empty", %{user: user} do
      {:ok, user} = Account.update(user.id, %{name: ""})

      new_name = Name.first_name()

      result =
        run_query(@query, user, %{
          "values" => %{"first_name" => new_name}
        })

      refute has_errors(result)

      assert result.data == %{
               "userUpdate" => %{
                 "successful" => true,
                 "result" => %{
                   "id" => user.id
                 }
               }
             }

      u = Repo.get(User, user.id)
      assert Account.last_name(u) == new_name
      assert Account.first_name(u) == ""
    end

    @query """
    mutation ($first_name: String, $last_name: String, $name: String) {
      userUpdate (input: {values:
          {firstName: $first_name, lastName: $last_name, name: $name}}) {
        successful
      }
    }
    """
    test "set a user's first name", %{user: user} do
      first_name = Name.first_name()
      result = run_query(@query, user, %{"first_name" => first_name})

      refute has_errors(result)

      assert User
             |> Repo.get(user.id)
             |> Account.first_name() == first_name
    end

    test "set a user's last name", %{user: user} do
      last_name = Name.last_name()
      result = run_query(@query, user, %{"last_name" => last_name})

      refute has_errors(result)

      assert User
             |> Repo.get(user.id)
             |> Account.last_name() == last_name
    end

    test "set a user's first and last names", %{user: user} do
      first_name = Name.first_name()
      last_name = Name.last_name()

      result =
        run_query(@query, user, %{
          "first_name" => first_name,
          "last_name" => last_name
        })

      refute has_errors(result)

      user = Repo.get(User, user.id)
      assert Account.first_name(user) == first_name
      assert Account.last_name(user) == last_name
      assert user.name == first_name <> " " <> last_name
    end

    test "set a user's full name", %{user: user} do
      # The first and last names should be ignored and
      # overridden by the full name
      first_name = Name.first_name()
      last_name = Name.last_name()
      full_name = Name.name()

      result =
        run_query(@query, user, %{
          "first_name" => first_name,
          "last_name" => last_name,
          "name" => full_name
        })

      refute has_errors(result)

      assert Repo.get(User, user.id).name == full_name
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

    test "get user info anonymously with non-existant ID" do
      result = run_query(@query, nil, %{"id" => ID.new()})

      assert error_count(result) == 1

      assert error_msg(result) =~
               "This operation requires an authenticated user"

      assert result.data == %{"user" => nil}
    end
  end

  describe "push notification mutations" do
    @query """
    mutation ($input: PushNotificationsEnableInput!) {
      pushNotificationsEnable (input: $input) {
        successful
      }
    }
    """

    test "enable notifications with defaults", %{user: user} do
      device = Factory.device()
      token = ID.new()

      input = %{
        "device" => device,
        "token" => token
      }

      result = run_query(@query, user, %{"input" => input})

      refute has_errors(result)

      assert result.data == %{
               "pushNotificationsEnable" => %{
                 "successful" => true
               }
             }

      assert %Token{
               device: ^device,
               token: ^token,
               platform: :apns,
               dev_mode: false,
               valid: true
             } = Repo.get_by(Token, user_id: user.id)
    end

    test "enable notifications with no defaults", %{user: user} do
      device = Factory.device()
      token = ID.new()

      input = %{
        "device" => device,
        "token" => token,
        "platform" => "APNS",
        "devMode" => true
      }

      result = run_query(@query, user, %{"input" => input})

      refute has_errors(result)

      assert result.data == %{
               "pushNotificationsEnable" => %{
                 "successful" => true
               }
             }

      assert %Token{
               device: ^device,
               token: ^token,
               platform: :apns,
               dev_mode: true,
               valid: true
             } = Repo.get_by(Token, user_id: user.id)
    end

    @query """
    mutation ($input: PushNotificationsDisableInput!) {
      pushNotificationsDisable (input: $input) {
        successful
      }
    }
    """

    test "disable notifications", %{user: user} do
      device = Factory.device()

      Push.enable(user, device, ID.new())

      result = run_query(@query, user, %{"input" => %{"device" => device}})

      refute has_errors(result)

      assert result.data == %{
               "pushNotificationsDisable" => %{
                 "successful" => true
               }
             }

      assert %Token{
               device: ^device,
               valid: false
             } = Repo.get_by(Token, user_id: user.id)
    end
  end

  describe "location token mutation" do
    @query """
    mutation {
      userLocationGetToken {
        successful
        result
      }
    }
    """

    test "get location token", %{user: user} do
      result = run_query(@query, user, %{})

      refute has_errors(result)

      assert %{
               "userLocationGetToken" => %{
                 "successful" => true,
                 "result" => token
               }
             } = result.data

      assert is_binary(token)
    end
  end

  describe "live location sharing" do
    setup %{user: user, user2: user2} do
      Roster.befriend(user, user2)

      :ok
    end

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
              expiresAt
            }
          }
        }
      }
    }
    """

    test "get user's sharing sessions", %{user: user, user2: user2} do
      sharer = user.id
      shared_with = user2.id
      expiry = sharing_expiry()

      {:ok, share} = Location.start_sharing_location(user, user2, expiry)
      id = share.id

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
                         "expiresAt" => ^expiry
                       }
                     }
                   ],
                   "totalCount" => 1
                 }
               }
             } = result.data
    end

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
              expiresAt
            }
          }
        }
      }
    }
    """

    test "get sharing sessions with user", %{user: user, user2: user2} do
      sharer = user.id
      shared_with = user2.id
      expiry = sharing_expiry()

      {:ok, share} = Location.start_sharing_location(user, user2, expiry)
      id = share.id

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
                         "expiresAt" => ^expiry
                       }
                     }
                   ],
                   "totalCount" => 1
                 }
               }
             } = result.data
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
                     "field" => "sharedWithId",
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
      expiry = sharing_expiry()

      {:ok, _} = Location.start_sharing_location(user, user2, expiry)

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
      expiry = sharing_expiry()

      {:ok, _} = Location.start_sharing_location(user, user2, expiry)

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

  describe "contacts" do
    @query """
    query ($rel: UserContactRelationship) {
      currentUser {
        contacts (first: 1, relationship: $rel) {
          totalCount
          edges {
            relationship
            created_at
            node {
              id
            }
          }
        }
      }
    }
    """

    test "get contacts by relationship", %{user: user, user2: user2} do
      Roster.befriend(user, user2)
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
                         "created_at" => _
                       }
                     ],
                     "totalCount" => 1
                   }
                 }
               } = result.data
      end
    end

    test "get contacts by relationship 'none'", %{user: user} do
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
      code = Account.make_invite_code(inviter)

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
      assert Account.get_user(user.id) == nil
    end
  end

  describe "friend invite mutation" do
    @query """
    mutation ($userId: UUID!) {
      friendInvite(input: {userId: $userId}) {
        successful
        result
      }
    }
    """
    test "should make a invited user from a non-relationship", shared do
      result = run_query(@query, shared.user, %{"userId" => shared.user2.id})
      refute has_errors(result)

      assert result.data == %{
               "friendInvite" => %{
                 "successful" => true,
                 "result" => "INVITED"
               }
             }

      assert Roster.relationship(shared.user, shared.user2) == :invited
    end

    test "should make a friend from a invited_by", shared do
      Roster.invite(shared.user2, shared.user)
      result = run_query(@query, shared.user, %{"userId" => shared.user2.id})
      refute has_errors(result)

      assert result.data == %{
               "friendInvite" => %{
                 "successful" => true,
                 "result" => "FRIEND"
               }
             }

      assert Roster.relationship(shared.user, shared.user2) == :friend
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
      Roster.befriend(shared.user, shared.user2)
      result = run_query(@query, shared.user, %{"userId" => shared.user2.id})
      refute has_errors(result)

      assert result.data == %{
               "friendDelete" => %{
                 "successful" => true,
                 "result" => true
               }
             }

      assert Roster.relationship(shared.user, shared.user2) == :none
    end

    test "should remove all relationship with an invitee", shared do
      Roster.invite(shared.user, shared.user2)
      result = run_query(@query, shared.user, %{"userId" => shared.user2.id})
      refute has_errors(result)

      assert result.data == %{
               "friendDelete" => %{
                 "successful" => true,
                 "result" => true
               }
             }

      assert Roster.relationship(shared.user, shared.user2) == :none
    end

    test "should remove all relationship with an invited_by", shared do
      Roster.invite(shared.user2, shared.user)
      result = run_query(@query, shared.user, %{"userId" => shared.user2.id})
      refute has_errors(result)

      assert result.data == %{
               "friendDelete" => %{
                 "successful" => true,
                 "result" => true
               }
             }

      assert Roster.relationship(shared.user, shared.user2) == :none
    end

    test "should return an error for a blocked user", shared do
      Block.block(shared.user2, shared.user)
      result = run_query(@query, shared.user, %{"userId" => shared.user2.id})
      assert has_errors(result)
      assert error_msg(result) =~ "Invalid user"
    end

    test "should return an error if you try to unfriend yourself", shared do
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

  describe "freinds connection" do
    @query """
    query {
      currentUser {
        friends (first: 1) {
          totalCount
          edges {
            node {
              user { id }
              name
              created_at
            }
          }
        }
      }
    }
    """

    test "get friends", %{user: user, user2: user2} do
      Roster.befriend(user, user2)
      name = Name.name()
      Roster.set_name(user, user2, name)
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
                         "created_at" => _
                       }
                     }
                   ],
                   "totalCount" => 1
                 }
               }
             } = result.data
    end
  end

  describe "sent_invitations connection" do
    @query """
    query {
      currentUser {
        sentInvitations (first: 1) {
          totalCount
          edges {
            node {
              sender { id }
              recipient { id }
              created_at
            }
          }
        }
      }
    }
    """

    test "get sent_invitations", %{user: user, user2: user2} do
      Roster.invite(user, user2)
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
                         "created_at" => _
                       }
                     }
                   ],
                   "totalCount" => 1
                 }
               }
             } = result.data
    end
  end

  describe "received_invitations connection" do
    @query """
    query {
      currentUser {
        receivedInvitations (first: 1) {
          totalCount
          edges {
            node {
              sender { id }
              recipient { id }
              created_at
            }
          }
        }
      }
    }
    """

    test "get received_invitations", %{user: user, user2: user2} do
      Roster.invite(user2, user)
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
                         "created_at" => _
                       }
                     }
                   ],
                   "totalCount" => 1
                 }
               }
             } = result.data
    end
  end

  describe "naming a friend" do
    @query """
    mutation ($user_id: UUID!, $name: String!) {
      friendName (input: {user_id: $user_id, name: $name}) {
        successful
        result
      }
    }
    """

    test "assign a name to a friend", %{user: user, user2: user2} do
      Roster.befriend(user, user2)
      new_name = Name.name()

      result =
        run_query(@query, user, %{"user_id" => user2.id, "name" => new_name})

      refute has_errors(result)

      assert result.data == %{
               "friendName" => %{
                 "successful" => true,
                 "result" => true
               }
             }

      assert Roster.get_item(user, user2).name == new_name
    end

    test "should fail when the user doesn't exist", %{user: user} do
      new_name = Name.name()

      result =
        run_query(@query, user, %{"user_id" => ID.new(), "name" => new_name})

      assert has_errors(result)
      assert error_msg(result) =~ "User not found"
    end

    test "should fail when the user is not a friend", %{
      user: user,
      user2: user2
    } do
      new_name = Name.name()

      result =
        run_query(@query, user, %{"user_id" => user2.id, "name" => new_name})

      assert has_errors(result)
      assert error_msg(result) =~ "User not found"
    end
  end
end
