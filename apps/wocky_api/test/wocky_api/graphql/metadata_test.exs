defmodule WockyAPI.GraphQL.MetadataTest do
  use WockyAPI.GraphQLCase, async: true

  alias Wocky.Repo.Factory

  setup do
    user = Factory.insert(:user)
    metadata = Factory.insert_list(2, :metadata)

    {:ok, user: user, metadata: metadata}
  end

  describe "metadata retrieval" do
    @query """
    query {
      metadata {
        key
        value
        description
      }
    }
    """

    test "should retrieve metadata", %{user: user, metadata: [md1, md2]} do
      v = to_string(Application.spec(:wocky)[:vsn])
      result = run_query(@query, user)
      refute has_errors(result)

      [
        %{
          "key" => md1.key,
          "value" => md1.value,
          "description" => md1.description
        },
        %{
          "key" => md2.key,
          "value" => md2.value,
          "description" => md2.description
        },
        %{
          "key" => "server_version",
          "value" => v,
          "description" => "Current version of the Wocky server"
        }
      ]
      |> Enum.each(fn m -> assert Enum.member?(result.data["metadata"], m) end)
    end
  end
end
