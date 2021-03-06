defmodule WockyAPI.GraphQL.AuthenticationTest do
  use WockyAPI.GraphQLCase, async: true

  alias Faker.Lorem
  alias WockyAPI.Factory, as: APIFactory

  @query """
  mutation ($token: String!) {
  authenticate (input: {token: $token}) {
      user {
        id
      }
    }
  }
  """

  describe "GraphQL in-band JWT authentication" do
    setup do
      user = Factory.insert(:user)
      jwt = APIFactory.get_test_token(user)
      {:ok, user: user, jwt: jwt}
    end

    test "successful authentication", %{user: user, jwt: jwt} do
      result = run_query(@query, nil, %{"token" => jwt})

      refute has_errors(result)
      assert result.data == %{"authenticate" => %{"user" => %{"id" => user.id}}}
    end

    test "re-authentication should not be permitted", %{user: user, jwt: jwt} do
      result = run_query(@query, user, %{"token" => jwt})

      assert error_count(result) == 1
      assert error_msg(result) =~ "already authenticated"
      assert result.data == %{"authenticate" => nil}
    end

    test "unsuccessful authentication" do
      result = run_query(@query, nil, %{"token" => Lorem.word()})

      assert error_count(result) == 1
      assert error_msg(result) =~ "invalid user"
      assert result.data == %{"authenticate" => nil}
    end

    test "empty device", %{user: user} do
      jwt = APIFactory.get_test_token(user, %{"dvc" => ""})
      result = run_query(@query, nil, %{"token" => jwt})

      assert error_count(result) == 1
      assert error_msg(result) =~ "invalid user token"
      assert result.data == %{"authenticate" => nil}
    end
  end
end
