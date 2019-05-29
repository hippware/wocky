defmodule WockyAPI.GraphQL.TestingTest do
  use WockyAPI.GraphQLCase, async: false

  alias Wocky.Location.Share
  alias Wocky.{Repo, User}
  alias Wocky.Repo.Factory

  setup do
    {:ok, user: Factory.insert(:user)}
  end

  describe "factory insert" do
    @query """
    mutation ($input: FactoryInsertInput) {
      factoryInsert (input: $input) {
        result
        successful
        messages {
          message
        }
      }
    }
    """

    test "should insert a user", %{user: user} do
      number = Factory.phone_number()

      result =
        run_query(@query, user, %{
          "input" => [
            %{
              "type" => "user",
              "string_params" => [%{"key" => "phone_number", "value" => number}]
            }
          ]
        })

      refute has_errors(result)

      assert Repo.get_by(User, phone_number: number)
    end

    test "should insert multiple bots", %{user: user} do
      result =
        run_query(@query, user, %{
          "input" => [
            %{
              "type" => "bot",
              "count" => 10,
              "string_params" => [%{"key" => "user_id", "value" => user.id}]
            }
          ]
        })

      refute has_errors(result)

      assert length(User.get_owned_bots(user)) == 10
    end

    test "should allow multiple types", %{user: user} do
      number = Factory.phone_number()

      result =
        run_query(@query, user, %{
          "input" => [
            %{
              "type" => "user",
              "string_params" => [%{"key" => "phone_number", "value" => number}]
            },
            %{
              "type" => "user_location_share",
              "string_params" => [
                %{"key" => "user_id", "value" => user.id},
                %{"key" => "shared_with_id", "value" => user.id}
              ]
            }
          ]
        })

      refute has_errors(result)

      assert Repo.get_by(User, phone_number: number)
      assert Repo.get_by(Share, user_id: user.id)
    end

    test "should fail to insert with improper params", %{user: user} do
      result =
        run_query(@query, user, %{
          "input" => [
            %{
              "type" => "bot",
              "count" => 10,
              "int_params" => [%{"key" => "user_id", "value" => user.id}]
            }
          ]
        })

      assert has_errors(result)

      assert User.get_owned_bots(user) == []
    end
  end

  describe "disable factory" do
    setup do
      orig_val = Confex.get_env(:wocky_api, :allow_factory_insert)
      Application.delete_env(:wocky_api, :allow_factory_insert)

      on_exit(fn ->
        Application.put_env(:wocky_api, :allow_factory_insert, orig_val)
      end)
    end

    test "should not run when not configured", %{user: user} do
      number = Factory.phone_number()

      result =
        run_query(@query, user, %{
          "input" => %{
            "type" => "user",
            "string_params" => [%{"key" => "phone_number", "value" => number}]
          }
        })

      assert has_errors(result)

      refute Repo.get_by(User, phone_number: number)
    end
  end
end
