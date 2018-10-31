defmodule WockyAPI.GraphQL.BlockTest do
  use WockyAPI.GraphQLCase, async: true

  alias Wocky.Block
  alias Wocky.Repo.{Factory, ID}

  setup do
    [user1, user2] = Factory.insert_list(2, :user)

    {:ok, user1: user1, user2: user2}
  end

  describe "blocking" do
    @query """
    mutation ($input: UserBlockInput) {
      userBlock (input: $input) {
        result
        successful
      }
    }
    """

    test "should block a user", %{user1: user1, user2: user2} do
      result = run_query(@query, user1, %{"input" => %{"user_id" => user2.id}})

      refute has_errors(result)

      assert Block.blocked?(user1, user2)
    end

    test "should have no effect on a blocked user", %{user1: user1, user2: user2} do
      Block.block(user1, user2)

      result = run_query(@query, user1, %{"input" => %{"user_id" => user2.id}})

      refute has_errors(result)

      assert Block.blocked?(user1, user2)
    end

    test "should fail to block a non-existant user", %{user1: user1} do
      result = run_query(@query, user1, %{"input" => %{"user_id" => ID.new()}})

      assert has_errors(result)

      assert error_msg(result) =~ "Invalid user"
    end
  end

  describe "unblocking" do
    @query """
    mutation ($input: UserUnblockInput) {
      userUnblock (input: $input) {
        result
        successful
      }
    }
    """

    test "should unblock a blocked user", %{user1: user1, user2: user2} do
      Block.block(user1, user2)

      result = run_query(@query, user1, %{"input" => %{"user_id" => user2.id}})

      refute has_errors(result)

      refute Block.blocked?(user1, user2)
    end

    test "should have no effect on an unblocked user", %{user1: user1, user2: user2} do
      result = run_query(@query, user1, %{"input" => %{"user_id" => user2.id}})

      refute has_errors(result)

      refute Block.blocked?(user1, user2)
    end

    test "should fail for a non-existant user", %{user1: user1} do
      result = run_query(@query, user1, %{"input" => %{"user_id" => ID.new()}})

      assert has_errors(result)

      assert error_msg(result) =~ "Invalid user"
    end
  end

  describe "get blocked users" do

    test "should get users blocked by the requestor",
    %{user1: user1, user2: user2} do
      query = """
      query {
        currentUser {
          blocks (first: 10) {
            edges {
              node {
                user_id
                handle
              }
            }
          }
        }
      }
      """
      user3 = Factory.insert(:user)

      Block.block(user1, user2)
      Block.block(user1, user3)

      result = run_query(query, user1)

      refute has_errors(result)

      assert %{
        "currentUser" => %{
          "blocks" => %{
            "edges" => [
              %{
                "node" => %{
                  "handle" => user3.handle,
                  "user_id" => user3.id
                }
              },
              %{
                "node" => %{
                  "handle" => user2.handle,
                  "user_id" => user2.id
                }
              }
            ]
          }
        }
      } == result.data
    end

    test "should not allow any other fields on blocked users to be retrived",
    %{user1: user1} do
      query = """
      query {
        currentUser {
          blocks (first: 10) {
            edges {
              node {
                avatar
              }
            }
          }
        }
      }
      """

      result = run_query(query, user1)

      assert has_errors(result)

      assert error_msg(result) =~ "Cannot query field"
    end

  end
end
