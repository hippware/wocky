defmodule WockyAPI.GraphQL.MediaTest do
  use WockyAPI.GraphQLCase, async: false

  alias Faker.Lorem
  alias Wocky.Repo
  alias Wocky.Repo.Factory
  alias Wocky.Repo.ID
  alias Wocky.TROS
  alias Wocky.TROS.Metadata

  setup do
    {:ok, user: Factory.insert(:user)}
  end

  describe "upload" do
    @query """
    mutation ($input: MediaUploadParams!) {
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
        "mimeType" => "image/png",
        "access" => Lorem.sentence()
      }

      result = run_query(@query, user, %{"input" => input})

      refute has_errors(result)

      assert %{"mediaUpload" => %{"result" => results}} = result.data

      assert ID.valid?(results["id"])
      assert results["upload_url"] =~ ~r"https?://.*"
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

    test "request upload with invalid mime type", %{user: user} do
      input = %{
        "filename" => Lorem.word(),
        "size" => 1000,
        "mimeType" => "aspect"
      }

      result = run_query(@query, user, %{"input" => input})

      assert error_msg(result) =~ ~r/Invalid MIME type/
    end

  end

  describe "delete" do
    setup %{user: user} do
      metadata = Factory.insert(:tros_metadata, user: user)
      url = TROS.make_url(metadata.id)
      {:ok, metadata: metadata, url: url}
    end

    @query """
    mutation ($input: MediaDeleteParams!) {
      mediaDelete (input: $input) {
        result
      }
    }
    """
    test "delete a file", %{user: user, metadata: metadata, url: url} do
      result = run_query(@query, user, %{"input" => %{"url" => url}})

      refute has_errors(result)

      assert %{"mediaDelete" => %{"result" => true}} = result.data

      assert TROS.get_metadata(metadata.id) == {:error, :not_found}
    end

    test "delete an unowned file", %{url: url} do
      result =
        run_query(@query, Factory.insert(:user), %{
          "input" => %{"url" => url}
        })

      assert error_msg(result) =~ "Permission denied"
    end

    test "delete a non-existant file", %{user: user} do
      url = TROS.make_url(ID.new())
      result = run_query(@query, user, %{"input" => %{"url" => url}})

      assert error_msg(result) =~ "File not found"
    end
  end

  describe "mediaUrls query" do
    setup do
      require_watcher()
      Wocky.Callbacks.TROSMetadata.register()

      metadata = Factory.insert(:tros_metadata, ready: false)
      tros_url = TROS.make_url(metadata.id)
      user = Factory.insert(:user)

      {:ok, user: user, metadata: metadata, tros_url: tros_url}
    end

    @query """
    query ($tros_url: String!, $timeout: Int) {
      media_urls (tros_url: $tros_url, timeout: $timeout) {
        full_url
        thumbnail_url
      }
    }
    """
    test "it should return URLs when the file is already ready", ctx do
      ctx.metadata |> Metadata.changeset(%{ready: true}) |> Repo.update()

      result =
        run_query(@query, ctx.user, %{
          "tros_url" => ctx.tros_url,
          "timeout" => 0
        })

      refute has_errors(result)

      assert result.data == %{
               "media_urls" => %{
                 "full_url" => "https://localhost/" <> ctx.metadata.id,
                 "thumbnail_url" =>
                   "https://localhost/" <> ctx.metadata.id <> "-thumbnail"
               }
             }
    end

    test """
         it should return URLs when the file becomes ready in the timeout preiod
         """,
         ctx do
      Task.start(fn ->
        Process.sleep(100)
        ctx.metadata |> Metadata.changeset(%{ready: true}) |> Repo.update!()
      end)

      result =
        run_query(@query, ctx.user, %{
          "tros_url" => ctx.tros_url,
          "timeout" => 1000
        })

      refute has_errors(result)

      assert result.data == %{
               "media_urls" => %{
                 "full_url" => "https://localhost/" <> ctx.metadata.id,
                 "thumbnail_url" =>
                   "https://localhost/" <> ctx.metadata.id <> "-thumbnail"
               }
             }
    end

    test """
         it should return a timeout if the file does not become ready in the
         timeout period
         """,
         ctx do
      result =
        run_query(@query, ctx.user, %{
          "tros_url" => ctx.tros_url,
          "timeout" => 5
        })

      assert has_errors(result)
      assert error_msg(result) =~ "Timeout"
    end

    @query """
    query ($tros_url: String!, $timeout: Int) {
      media_urls (tros_url: $tros_url, timeout: $timeout) {
        urls {
          type
          url
        }
      }
    }
    """
    test "it should return all available urls for an image", ctx do
      ctx.metadata |> Metadata.changeset(%{ready: true}) |> Repo.update()

      result =
        run_query(@query, ctx.user, %{
          "tros_url" => ctx.tros_url,
          "timeout" => 0
        })

      refute has_errors(result)

      assert %{
               "media_urls" => %{
                 "urls" => urls
               }
             } = result.data

      ["FULL", "THUMBNAIL"]
      |> Enum.each(fn f ->
        assert Enum.any?(urls, fn %{"type" => format} -> format == f end)
      end)

      assert Enum.count(urls) == 2

      assert Enum.all?(urls, fn %{"url" => url} ->
               url =~ "https://localhost"
             end)
    end
  end
end
