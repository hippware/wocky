defmodule Wocky.TROSSpec do
  use ESpec, async: true
  use Wocky.JID

  alias Wocky.Repo
  alias Wocky.Repo.Factory
  alias Wocky.Repo.ID
  alias Wocky.TROS
  alias Wocky.User

  let :id, do: ID.new
  let :tros_jid, do: TROS.make_jid("server", "file_id")
  let :tros_jid_u, do: TROS.make_jid("user", "server", "file_id")

  describe "make_jid/2" do
    subject do: JID.to_binary(tros_jid())
    it do: should(eq "server/file/file_id")
  end

  describe "make_jid/3" do
    subject do: JID.to_binary(tros_jid_u())
    it do: should(eq "user@server/file/file_id")
  end

  describe "parse_url/1" do
    it do: TROS.parse_url("bogus") |> should(be_error_result())
    it do: TROS.parse_url("tros:bogus") |> should(be_error_result())
    it do: TROS.parse_url("tros:localhost/foo") |> should(be_error_result())
    it do: TROS.parse_url("tros:localhost/file/foo") |> should(be_ok_result())
  end

  describe "make_url/1" do
    context "when passed a JID with no user" do
      subject do: TROS.make_url(tros_jid())
      it do: should(eq TROS.make_url("server", "file_id"))
      it do: should(eq "tros:server/file/file_id")
    end

    context "when passed a JID with a user" do
      subject do: TROS.make_url(tros_jid_u())
      it do: should(eq "tros:user@server/file/file_id")
    end
  end

  describe "make_url/2" do
    subject do: TROS.make_url("server", "file_id")
    it do: should(eq TROS.make_url(tros_jid()))
    it do: should(eq "tros:server/file/file_id")
  end

  describe "get_base_id/1" do
    it do: TROS.get_base_id("file-original") |> should(eq "file")
    it do: TROS.get_base_id("file-thumbnail") |> should(eq "file")
    it do: TROS.get_base_id("file") |> should(eq "file")
  end

  describe "get_type/1" do
    it do: TROS.get_type("file-original") |> should(eq :original)
    it do: TROS.get_type("file-thumbnail") |> should(eq :thumbnail)
    it do: TROS.get_type("file") |> should(eq :full)
  end

  describe "variants/1" do
    subject do: TROS.variants("file")
    it do: should(have_count 3)
    it do: should(have "file-thumbnail")
    it do: should(have "file-original")
    it do: should(have "file")
  end

  describe "database interactions" do
    before do
      user = Factory.insert(:user)
      md = Factory.insert(:tros_metadata, access: "all", user: user)
      {:ok, id: md.id, owner: user.id, md: md, user: user}
    end

    describe "get_metadata/1" do
      it do
        shared.id
        |> TROS.get_metadata
        |> should(be_ok_result())
      end

      it do
        shared.id
        |> TROS.get_metadata
        |> elem(1)
        |> Repo.preload(:user)
        |> should(eq shared.md)
      end

      it do: TROS.get_metadata(ID.new) |> should(be_error_result())
    end

    describe "update_access/2" do
      it do: TROS.update_access(shared.id, "access") |> should(be_ok_result())
    end

    describe "delete/2" do
      it do: TROS.delete(shared.server, shared.id) |> should(eq :ok)
    end

    describe "make_upload_response/5" do
      let :owner_jid, do: User.to_jid(shared.user)
      subject do: TROS.make_upload_response(owner_jid(), ID.new, 100, "all", [])
      it do: assert {_, _} = subject()
    end

    describe "make_download_response/2" do
      let :owner_jid, do: User.to_jid(shared.user)
      subject do: TROS.make_download_response("localhost", shared.id)
      it do: assert {_, _} = subject()
    end
  end
end
