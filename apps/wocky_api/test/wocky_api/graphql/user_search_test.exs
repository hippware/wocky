defmodule WockyAPI.GraphQL.UserSearchTest do
  use WockyAPI.GraphQLCase, async: false

  alias Wocky.Repo.Factory

  describe "user search" do
    setup do
      {:ok, user: Factory.insert(:user)}
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
end
