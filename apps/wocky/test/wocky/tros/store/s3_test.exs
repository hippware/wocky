defmodule Wocky.TROS.Store.S3Test do
  use Wocky.DataCase, async: true

  alias Faker.Lorem
  alias Plug.Conn
  alias Wocky.Repo.Factory
  alias Wocky.Repo.ID
  alias Wocky.TROS
  alias Wocky.TROS.Store.S3, as: S3Store

  @test_file "d49ff638-4736-11e7-8017-0e6514633f23"
  @url_re ~r/https?:\/\/.*/

  setup_all do
    Application.put_env(:wocky, :tros_s3_secret_key, "1234")
    Application.put_env(:wocky, :tros_s3_access_key_id, "1234")
  end

  describe "make_upload_response/4" do
    setup do
      owner = Factory.build(:user)
      file_id = ID.new()
      size = :rand.uniform(10_000)
      metadata = %{content_type: Lorem.word()}
      reference_url = TROS.make_url(owner, file_id)

      {headers, fields} =
        S3Store.make_upload_response(reference_url, file_id, size, metadata)

      {:ok,
       headers: headers,
       fields: fields,
       size: size,
       owner_id: owner.id,
       file_id: file_id}
    end

    test "should return appropriate headers and fields", ctx do
      assert :proplists.get_value("content-length", ctx.headers) ==
               to_string(ctx.size)

      assert :proplists.get_value("x-amz-content-sha256", ctx.headers) ==
               "UNSIGNED-PAYLOAD"

      assert :proplists.get_value("reference_url", ctx.fields) ==
               "tros:#{ctx.owner_id}@#{Wocky.host()}/file/#{ctx.file_id}"

      assert :proplists.get_value("url", ctx.fields) =~ @url_re

      assert :proplists.get_value("method", ctx.fields) == "PUT"
    end
  end

  describe "get_download_url/2" do
    test "should return a valid URL when the file is ready" do
      image = Factory.insert(:tros_metadata)
      assert S3Store.get_download_url(image, image.id) =~ @url_re
    end

    test "should return an empty URL when the file is not ready" do
      image = Factory.insert(:tros_metadata, ready: false)
      assert S3Store.get_download_url(image, image.id) == ""
    end
  end

  describe "delete/1" do
    setup do
      bypass = Bypass.open()
      {:ok, bypass: bypass}
    end

    test "should return :ok on success", context do
      setup_server(context.bypass)

      Bypass.expect(context.bypass, fn conn ->
        assert "DELETE" == conn.method
        Conn.resp(conn, 204, "")
      end)

      assert :ok = S3Store.delete(@test_file)
    end
  end

  describe "do_delete/1" do
    setup do
      bypass = Bypass.open()
      {:ok, bypass: bypass}
    end

    test "should succeed with valid credentials", context do
      setup_server(context.bypass)

      Bypass.expect(context.bypass, fn conn ->
        assert "DELETE" == conn.method
        Conn.resp(conn, 204, "")
      end)

      assert {:ok, _} = S3Store.do_delete(@test_file)
    end

    test "should return an error if the server rejects the auth", context do
      setup_server(context.bypass)

      Bypass.expect(context.bypass, fn conn ->
        Conn.resp(conn, 401, "")
      end)

      assert {:error, _} = S3Store.do_delete(@test_file)
    end
  end

  defp setup_server(bypass) do
    Application.put_env(:wocky, :s3_host_override, "localhost:#{bypass.port}")
  end
end
