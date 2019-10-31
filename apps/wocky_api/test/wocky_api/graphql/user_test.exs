defmodule WockyAPI.GraphQL.UserTest do
  use WockyAPI.GraphQLCase, async: true

  alias Faker.Lorem
  alias Faker.Name
  alias Wocky.Account
  alias Wocky.Account.User
  alias Wocky.Block
  alias Wocky.Notifier.Push
  alias Wocky.Notifier.Push.Token
  alias Wocky.Repo
  alias Wocky.Repo.Factory
  alias Wocky.Repo.ID

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
end
