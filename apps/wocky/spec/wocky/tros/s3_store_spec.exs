defmodule Wocky.TROS.S3StoreSpec do
  use ESpec, async: true

  alias Faker.Lorem
  alias Wocky.JID
  alias Wocky.Repo.ID
  alias Wocky.TROS.S3Store

  @url_re ~r/https?:\/\/.*/

  before_all do
    Application.put_env(:wocky, :tros_s3_secret_key, "1234")
    Application.put_env(:wocky, :tros_s3_access_key_id, "1234")
  end

  describe "make_download_response/2" do
    before do
      {headers, fields} = S3Store.make_download_response(shared.server, ID.new)
      {:ok,
        headers: headers,
        fields: fields
      }
    end

    it "should return an empty header list and a url" do
      shared.headers |> should(eq [])
      shared.fields |> should(have_length 1)
      :proplists.get_value("url", shared.fields) |> should(match @url_re)
    end
  end

  describe "make_upload_response/4" do
    before do
      owner_id = ID.new
      owner_jid = JID.make(owner_id, shared.server, "")
      file_id = ID.new
      size = :rand.uniform(10000)
      metadata = %{"content-type": Lorem.word}

      {headers, fields}
      = S3Store.make_upload_response(owner_jid, file_id, size, metadata)

      {:ok,
        headers: headers,
        fields: fields,
        size: size,
        owner_id: owner_id,
        file_id: file_id
      }
    end

    it "should return appropriate headers and fields" do
      :proplists.get_value("content-length", shared.headers)
      |> should(eq Integer.to_string(shared.size))

      :proplists.get_value("x-amz-content-sha256", shared.headers)
      |> should(eq "UNSIGNED-PAYLOAD")

      :proplists.get_value("reference_url", shared.fields)
      |> should(
        eq "tros:#{shared.owner_id}@#{shared.server}/file/#{shared.file_id}")

      :proplists.get_value("url", shared.fields)
      |> should(match @url_re)

      :proplists.get_value("method", shared.fields) |> should(eq "PUT")
    end

  end

end
