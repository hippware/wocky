defmodule WockyAPI.GraphQL.AuthenticationTest do
  use WockyAPI.GraphQLCase

  alias Faker.Lorem
  alias Wocky.Account
  alias Wocky.Account.ClientJWT
  alias Wocky.Repo.Factory

  @query """
  mutation ($user: UUID, $token: String!) {
  authenticate (input: {user: $user, token: $token}) {
      user {
        id
      }
    }
  }
  """

  describe "GraphQL in-band token authentication" do
    setup do
      user = Factory.insert(:user)
      {:ok, {token, _}} = Account.assign_token(user.id, "abc")
      {:ok, user: user, token: token}
    end

    test "successful authentication", %{user: user, token: token} do
      result = run_query(@query, nil, %{"user" => user.id, "token" => token})

      refute has_errors(result)
      assert result.data == %{"authenticate" => %{"user" => %{"id" => user.id}}}
    end

    test "unsuccessful authentication", %{user: user} do
      result =
        run_query(@query, nil, %{
          "user" => user.id,
          "token" => Lorem.word()
        })

      assert error_count(result) == 1
      assert error_msg(result) =~ "invalid user"
      assert result.data == %{"authenticate" => nil}
    end
  end

  describe "GraphQL in-band JWT authentication" do
    setup do
      user = Factory.insert(:user)
      {:ok, jwt, _} = ClientJWT.encode_and_sign(user)
      {:ok, user: user, jwt: jwt}
    end

    test "successful authentication", %{user: user, jwt: jwt} do
      result = run_query(@query, nil, %{"token" => jwt})

      refute has_errors(result)
      assert result.data == %{"authenticate" => %{"user" => %{"id" => user.id}}}
    end

    test "unsuccessful authentication" do
      result = run_query(@query, nil, %{"token" => Lorem.word()})

      assert error_count(result) == 1
      assert error_msg(result) =~ "invalid user"
      assert result.data == %{"authenticate" => nil}
    end
  end
end
