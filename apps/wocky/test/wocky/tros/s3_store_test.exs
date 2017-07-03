defmodule Wocky.TROS.S3StoreTest do
  use ExUnit.Case, async: true

  alias Plug.Conn
  alias Wocky.TROS.S3Store

  @test_file "d49ff638-4736-11e7-8017-0e6514633f23"

  describe "do_delete/2" do
    setup do
      bypass = Bypass.open
      {:ok,
        bypass: bypass,
        server: "localhost"
      }

    end

    test "should succeed with valid credentials", context do
      setup_server(context.bypass)
      Bypass.expect(context.bypass,
                    fn conn ->
                      assert "DELETE" == conn.method
                      Conn.resp(conn, 204, "")
                    end)
      {:ok, _} = S3Store.do_delete(context.server, @test_file)
    end

    test "should return an error if the server rejects the auth", context do
      setup_server(context.bypass)
      Bypass.expect(context.bypass,
                    fn conn ->
                      Conn.resp(conn, 401, "")
                    end)
      {:error, _} = S3Store.do_delete(context.server, @test_file)
    end

  end

  # make_upload_resonse/4 and make_download_response/2 are tested in the
  # corresponding espec file.

  defp setup_server(bypass) do
      Application.put_env(:wocky, :s3_host_override,
                          "localhost:#{bypass.port}")
  end
end
