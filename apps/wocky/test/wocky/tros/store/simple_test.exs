defmodule Wocky.TROS.Store.SimpleTest do
  use Wocky.DataCase, async: true

  alias Faker.Lorem
  alias Wocky.TROS
  alias Wocky.TROS.Store.Simple, as: SimpleStore

  @url_re ~r/https?:\/\/.*/

  describe "make_upload_response/4" do
    setup do
      owner = Factory.insert(:user)
      md = Factory.insert(:tros_metadata, access: "all", user: owner)

      size = :rand.uniform(10_000)
      metadata = %{content_type: Lorem.word()}
      reference_url = TROS.make_url(owner, md.id)

      {headers, fields} =
        SimpleStore.make_upload_response(reference_url, md.id, size, metadata)

      {:ok,
       headers: headers,
       fields: fields,
       size: size,
       owner_id: owner.id,
       file_id: md.id}
    end

    test "should return appropriate headers and fields", ctx do
      assert :proplists.get_value("method", ctx.fields) == "PUT"

      assert :proplists.get_value("url", ctx.fields) =~ @url_re

      assert :proplists.get_value("reference_url", ctx.fields) ==
               "tros:#{ctx.owner_id}@#{Wocky.host()}/file/#{ctx.file_id}"
    end
  end

  describe "get_download_url/2" do
    test "should return a valid URL" do
      image = Factory.insert(:tros_metadata)
      assert SimpleStore.get_download_url(image, image.id) =~ @url_re
    end
  end
end
