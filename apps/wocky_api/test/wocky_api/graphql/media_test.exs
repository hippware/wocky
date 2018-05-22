defmodule WockyAPI.GraphQL.MediaTest do
  use WockyAPI.GraphQLCase, async: true

  alias Faker.Lorem
  alias Wocky.Repo.Factory
  alias Wocky.Repo.ID

  setup do
    {:ok, user: Factory.insert(:user)}
  end

  describe "upload" do
    @query """
    mutation ($input: MediaUploadParams) {
      mediaUpload (input: $input) {
        result {
          id
          upload_url
          method
          headers {
            name
            value
          }
          reference_url
        }
      }
    }
    """
    test "request an upload of a file", %{user: user} do
      input = %{
        "filename" => Lorem.word(),
        "size" => :rand.uniform(1000),
        "mimeType" => Lorem.word(),
        "access" => Lorem.sentence()
      }

      result = run_query(@query, user, %{"input" => input})

      refute has_errors(result)

      assert %{"mediaUpload" => %{"result" => results}} = result.data

      assert ID.valid?(results["id"])
      assert results["upload_url"] =~ ~r"http://.*"
      assert results["method"] in ["PUT", "POST"]
      assert is_list(results["headers"])
      assert results["reference_url"] =~ ~r/tros:.*#{results["id"]}/
    end

    test "request upload with invalid params", %{user: user} do
      input = %{
        "filename" => nil,
        "size" => -50,
        "mimeType" => nil
      }

      result = run_query(@query, user, %{"input" => input})

      assert error_msg(result) =~ ~r/In field "filename"/
      assert error_msg(result) =~ ~r/In field "mimeType"/
    end
  end
end
