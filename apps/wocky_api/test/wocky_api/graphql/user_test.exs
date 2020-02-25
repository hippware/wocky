defmodule WockyAPI.GraphQL.UserTest do
  use WockyAPI.GraphQLCase, async: true

  alias Faker.Lorem
  alias Faker.Name
  alias Wocky.Account
  alias Wocky.Account.User
  alias Wocky.Contacts
  alias Wocky.Notifier.Push
  alias Wocky.Notifier.Push.Token
  alias Wocky.Repo
  alias Wocky.Repo.ID

  setup do
    [user, user2] = Factory.insert_list(2, :user)

    {:ok, user: user, user2: user2}
  end

  # -------------------------------------------------------------------
  # Queries

  describe "currentUser query" do
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
                 "updated_at" => DateTime.to_iso8601(user.updated_at)
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
  end

  describe "user query" do
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
      Contacts.block(user2, user)

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

  describe "userBulkLookup query" do
    setup do
      users = Factory.insert_list(5, :user)
      phone_numbers = Enum.map(users, & &1.phone_number)

      {:ok, users: users, phone_numbers: phone_numbers}
    end

    @query """
    query ($phone_numbers: [String!]) {
      userBulkLookup(phone_numbers: $phone_numbers) {
        phone_number
        e164_phone_number
        user { id }
        relationship
      }
    }
    """
    test "should return a list of supplied users", ctx do
      result =
        run_query(@query, ctx.user, %{"phone_numbers" => ctx.phone_numbers})

      refute has_errors(result)

      assert %{
               "userBulkLookup" => results
             } = result.data

      expected =
        ctx.users
        |> Enum.map(
          &%{
            "phone_number" => &1.phone_number,
            "e164_phone_number" => &1.phone_number,
            "user" => %{"id" => &1.id},
            "relationship" => "NONE"
          }
        )
        |> Enum.sort()

      assert expected == Enum.sort(results)
    end

    test "should return an empty user result set for unused numbers", ctx do
      numbers = unused_numbers(ctx.phone_numbers)
      result = run_query(@query, ctx.user, %{"phone_numbers" => numbers})

      refute has_errors(result)

      assert %{
               "userBulkLookup" => results
             } = result.data

      expected =
        numbers
        |> Enum.map(
          &%{
            "phone_number" => &1,
            "e164_phone_number" => &1,
            "user" => nil,
            "relationship" => nil
          }
        )
        |> Enum.sort()

      assert expected == Enum.sort(results)
    end

    test """
         should return a combination when some numbers are used and some unused
         """,
         ctx do
      unused_numbers = unused_numbers(ctx.phone_numbers)
      numbers = unused_numbers ++ ctx.phone_numbers
      result = run_query(@query, ctx.user, %{"phone_numbers" => numbers})

      refute has_errors(result)

      assert %{
               "userBulkLookup" => results
             } = result.data

      expected =
        Enum.map(
          unused_numbers,
          &%{
            "phone_number" => &1,
            "e164_phone_number" => &1,
            "user" => nil,
            "relationship" => nil
          }
        ) ++
          Enum.map(
            ctx.users,
            &%{
              "phone_number" => &1.phone_number,
              "e164_phone_number" => &1.phone_number,
              "user" => %{"id" => &1.id},
              "relationship" => "NONE"
            }
          )

      assert Enum.sort(expected) == Enum.sort(results)
    end

    test "should handle an empty request", ctx do
      result = run_query(@query, ctx.user, %{"phone_numbers" => []})

      refute has_errors(result)

      assert %{
               "userBulkLookup" => []
             } = result.data
    end

    test "should work when requestor's number is included", ctx do
      result =
        run_query(@query, ctx.user, %{
          "phone_numbers" => [ctx.user.phone_number]
        })

      refute has_errors(result)

      assert %{
               "userBulkLookup" => [
                 %{
                   "phone_number" => ctx.user.phone_number,
                   "e164_phone_number" => ctx.user.phone_number,
                   "user" => %{"id" => ctx.user.id},
                   "relationship" => "SELF"
                 }
               ]
             } == result.data
    end

    test "should not return blocked users", ctx do
      blocked = hd(ctx.users)
      Contacts.block(blocked, ctx.user)

      result =
        run_query(@query, ctx.user, %{"phone_numbers" => [blocked.phone_number]})

      refute has_errors(result)

      assert %{
               "userBulkLookup" => [
                 %{
                   "phone_number" => blocked.phone_number,
                   "e164_phone_number" => blocked.phone_number,
                   "user" => nil,
                   "relationship" => nil
                 }
               ]
             } == result.data
    end

    test "should normalise phone number to E.164", ctx do
      full_number = hd(ctx.phone_numbers)
      {"+1", number} = String.split_at(full_number, 2)

      result = run_query(@query, ctx.user, %{"phone_numbers" => [number]})

      refute has_errors(result)

      assert %{
               "userBulkLookup" => [
                 %{
                   "phone_number" => number,
                   "e164_phone_number" => full_number,
                   "user" => %{
                     "id" => hd(ctx.users).id
                   },
                   "relationship" => "NONE"
                 }
               ]
             } == result.data
    end

    test "should return the correct relationship for target users", ctx do
      [friend, inviter, invitee] = Enum.slice(ctx.users, 0..2)
      Contacts.befriend(ctx.user, friend)
      Contacts.make_friends(inviter, ctx.user, :disabled)
      Contacts.make_friends(ctx.user, invitee, :disabled)

      result =
        run_query(@query, ctx.user, %{
          "phone_numbers" => [
            friend.phone_number,
            inviter.phone_number,
            invitee.phone_number,
            ctx.user.phone_number
          ]
        })

      refute has_errors(result)

      assert %{
               "userBulkLookup" => results
             } = result.data

      expected = [
        %{
          "phone_number" => friend.phone_number,
          "e164_phone_number" => friend.phone_number,
          "user" => %{
            "id" => friend.id
          },
          "relationship" => "FRIEND"
        },
        %{
          "phone_number" => inviter.phone_number,
          "e164_phone_number" => inviter.phone_number,
          "user" => %{
            "id" => inviter.id
          },
          "relationship" => "INVITED_BY"
        },
        %{
          "phone_number" => invitee.phone_number,
          "e164_phone_number" => invitee.phone_number,
          "user" => %{
            "id" => invitee.id
          },
          "relationship" => "INVITED"
        },
        %{
          "phone_number" => ctx.user.phone_number,
          "e164_phone_number" => ctx.user.phone_number,
          "user" => %{
            "id" => ctx.user.id
          },
          "relationship" => "SELF"
        }
      ]

      assert Enum.sort(expected) == Enum.sort(results)
    end

    test "should fail when too many numbers are requested", ctx do
      result =
        run_query(@query, ctx.user, %{
          "phone_numbers" => unused_numbers(ctx.phone_numbers, 101)
        })

      assert has_errors(result)

      assert error_msg(result) =~ "Maximum bulk operation"
    end

    test "duplicate numbers with different formats should succeed", ctx do
      numbers = ["(580) 334-9474", "580-334-9474"]

      result =
        run_query(@query, ctx.user, %{
          "phone_numbers" => numbers
        })

      refute has_errors(result)

      e164 = "+15803349474"

      expected =
        numbers
        |> Enum.map(
          &%{
            "e164_phone_number" => e164,
            "phone_number" => &1,
            "user" => nil,
            "relationship" => nil
          }
        )
        |> Enum.sort()

      assert expected == Enum.sort(result.data["userBulkLookup"])
    end

    test "non-normalisable numbers should return empty results", ctx do
      numbers = ["xxxxx", hd(ctx.phone_numbers)]
      target = hd(ctx.users)

      result =
        run_query(@query, ctx.user, %{
          "phone_numbers" => numbers
        })

      refute has_errors(result)

      expected =
        [
          %{
            "e164_phone_number" => target.phone_number,
            "phone_number" => target.phone_number,
            "user" => %{"id" => target.id},
            "relationship" => "NONE"
          },
          %{
            "phone_number" => "xxxxx",
            "e164_phone_number" => nil,
            "user" => nil,
            "relationship" => nil
          }
        ]
        |> Enum.sort()

      assert expected == Enum.sort(result.data["userBulkLookup"])
    end

    test "should not produce errors on multiple idential input", ctx do
      [n] = unused_numbers(ctx.phone_numbers, 1)

      result =
        run_query(@query, ctx.user, %{
          "phone_numbers" => [n, n]
        })

      refute has_errors(result)

      assert %{
               "userBulkLookup" => [
                 %{
                   "e164_phone_number" => n,
                   "phone_number" => n,
                   "user" => nil,
                   "relationship" => nil
                 }
               ]
             } == result.data
    end
  end

  defp unused_numbers(phone_numbers, count \\ 5) do
    1..count
    |> Enum.map(fn _ -> Factory.phone_number() end)
    |> Enum.uniq()
    |> Kernel.--(phone_numbers)
  end

  # -------------------------------------------------------------------
  # User mutations

  describe "userUpdate mutation" do
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
      {:ok, user} = Account.update(user, %{name: ""})

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

  describe "userDelete mutation" do
    @query "mutation { userDelete { result } }"

    test "should be false with no related bots", %{user: user} do
      result = run_query(@query, user)
      refute has_errors(result)
      assert result.data == %{"userDelete" => %{"result" => true}}
      assert Account.get_user(user.id) == nil
    end
  end

  # -------------------------------------------------------------------
  # User location mutations

  describe "pushNotificationsEnable mutation" do
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
  end

  describe "pushNotificationsDisable mutation" do
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

  # -------------------------------------------------------------------
  # User location mutations

  describe "userLocationGetToken mutation" do
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

  # -------------------------------------------------------------------
  # Debug mutations

  describe "userFullAudit mutation" do
    @query """
    mutation ($input: UserFullAuditInput!) {
      userFullAudit (input: $input) {
        successful
      }
    }
    """

    @audit_types [:traffic, :location, :push, :push_payload]

    test "enable user auditing", %{user: user} do
      result = run_query(@query, user, %{"input" => %{"enable" => true}})

      refute has_errors(result)
      assert result.data["userFullAudit"]["successful"] == true

      Enum.each(@audit_types, fn t ->
        assert FunWithFlags.enabled?(t, for: user)
      end)
    end

    test "disable user auditing", %{user: user} do
      result = run_query(@query, user, %{"input" => %{"enable" => false}})

      refute has_errors(result)
      assert result.data["userFullAudit"]["successful"] == true

      Enum.each(@audit_types, fn t ->
        refute FunWithFlags.enabled?(t, for: user)
      end)
    end
  end
end
