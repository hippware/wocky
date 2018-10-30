defmodule Wocky.TROSTest do
  use Wocky.DataCase, async: true
  use Wocky.JID

  alias Wocky.Repo
  alias Wocky.Repo.Factory
  alias Wocky.Repo.ID
  alias Wocky.TROS
  alias Wocky.User

  setup do
    {:ok,
     id: ID.new(),
     tros_jid: TROS.make_jid("file_id"),
     tros_jid_u: TROS.make_jid("user", "file_id")}
  end

  test "make_jid/2", ctx do
    assert JID.to_binary(ctx.tros_jid) == "#{Wocky.host()}/file/file_id"
  end

  test "make_jid/3", ctx do
    assert JID.to_binary(ctx.tros_jid_u) == "user@#{Wocky.host()}/file/file_id"
  end

  describe "parse_url/1" do
    assert {:error, _} = TROS.parse_url("bogus")
    assert {:error, _} = TROS.parse_url("tros:bogus")
    assert {:error, _} = TROS.parse_url("tros:localhost/foo")

    assert {:ok, _} = TROS.parse_url("tros:localhost/file/foo")
  end

  describe "make_url/1" do
    test "when passed a JID with no user", ctx do
      url = TROS.make_url(ctx.tros_jid)
      assert url == TROS.make_url("file_id")
      assert url == "tros:#{Wocky.host()}/file/file_id"
    end

    test "when passed a JID with a user", ctx do
      assert TROS.make_url(ctx.tros_jid_u) ==
               "tros:user@#{Wocky.host()}/file/file_id"
    end
  end

  test "make_url/2", ctx do
    url = TROS.make_url("file_id")
    assert url == TROS.make_url(ctx.tros_jid)
    assert url == "tros:#{Wocky.host()}/file/file_id"
  end

  test "get_base_id/1" do
    assert TROS.get_base_id("file-original") == "file"
    assert TROS.get_base_id("file-thumbnail") == "file"
    assert TROS.get_base_id("file") == "file"
  end

  test "get_type/1" do
    assert TROS.get_type("file-original") == :original
    assert TROS.get_type("file-thumbnail") == :thumbnail
    assert TROS.get_type("file") == :full
  end

  test "variants/1" do
    variants = TROS.variants("file")

    assert length(variants) == 3
    assert Enum.member?(variants, "file-thumbnail")
    assert Enum.member?(variants, "file-original")
    assert Enum.member?(variants, "file")
  end

  describe "database interactions" do
    setup do
      user = Factory.insert(:user)
      md = Factory.insert(:tros_metadata, access: "all", user: user)
      {:ok, id: md.id, owner: user.id, md: md, user: user}
    end

    test "get_metadata/1", ctx do
      assert {:ok, md} = TROS.get_metadata(ctx.id)
      assert Repo.preload(md, :user) == ctx.md

      assert {:error, _} = TROS.get_metadata(ID.new())
    end

    test "update_access/2", ctx do
      assert {:ok, _} = TROS.update_access(ctx.id, "access")
    end

    test "delete/2", ctx do
      assert {:ok, _} = TROS.delete(ctx.id, ctx.user)
    end

    test "make_upload_response/5", ctx do
      owner_jid = User.to_jid(ctx.user)

      assert {:ok, {_, _}} =
               TROS.make_upload_response(owner_jid, ID.new(), 100, "all", [])
    end

    test "make_download_response/1", ctx do
      assert {:ok, _} = TROS.make_download_response(ctx.id)
    end
  end
end
