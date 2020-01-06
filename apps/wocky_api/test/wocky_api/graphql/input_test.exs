defmodule WockyAPI.GraphQL.InputTest do
  @moduledoc """
  A basic set of tests on the Big List of Naughty Strings to make sure they
  cause no issues for Absinthe or the database.

  We test against the handle, which has some server-side processing, and may
  end up being rejected, and the client_data which is untouched by the server
  and must always be accepted and returned verbatim.
  """
  use WockyAPI.GraphQLCase, async: true

  setup do
    user = Factory.insert(:user, handle: "abc")

    blns =
      "test/wocky_api/graphql/data/blns.json"
      |> File.read!()
      |> Jason.decode!()

    {:ok, user: user, blns: blns}
  end

  describe "current user" do
    @query """
    mutation ($values: UserUpdateInput!) {
      userUpdate (input: {values: $values}) {
        successful
        messages {
          field
          message
        }
        result {
          id
          clientData
        }
      }
    }
    """

    test "set user handle", ctx do
      Enum.each(ctx.blns, &test_set_handle(ctx.user, &1))
    end

    test "set client data", ctx do
      Enum.each(ctx.blns, &test_set_client_data(ctx.user, &1))
    end

    defp test_set_handle(user, handle) do
      result =
        run_query(@query, user, %{
          "values" => %{"handle" => handle}
        })

      refute has_errors(result)
    end

    defp test_set_client_data(user, data) do
      result =
        run_query(@query, user, %{
          "values" => %{"client_data" => data}
        })

      # For some reason Absinthe treats emtpy strings as null. I'm not sure it's
      # a big deal but we need to account for it here
      expected =
        case data do
          "" -> nil
          x -> x
        end

      refute has_errors(result)

      assert result.data == %{
               "userUpdate" => %{
                 "successful" => true,
                 "result" => %{
                   "id" => user.id,
                   "clientData" => expected
                 },
                 "messages" => []
               }
             }
    end
  end
end
