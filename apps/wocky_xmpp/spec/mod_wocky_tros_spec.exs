defmodule :mod_wocky_tros_spec do
  use ESpec, async: true, sandbox: true
  use SandboxHelper
  use IQHandlerSpec

  import :mod_wocky_tros, only: [handle_iq: 3]

  alias Wocky.Repo.Factory
  alias Wocky.Repo.ID
  alias Wocky.TROS
  alias Wocky.User

  @ns_tros "foo"
  @filename "photo of cat.jpg"

  before do
    alice = Factory.insert(:user, resource: "testing")
    bob = Factory.insert(:user)
    carol = Factory.insert(:user)

    bob_access = "user:" <> (bob |> User.to_jid() |> JID.to_binary())
    avatar = Factory.insert(:tros_metadata, user: alice, access: "all")
    media = Factory.insert(:tros_metadata, user: alice, access: bob_access)

    {:ok,
     [
       user_jid: User.to_jid(alice),
       alice: alice,
       bob: bob,
       carol: carol,
       bob_access: bob_access,
       avatar: avatar,
       media: media
     ]}
  end

  describe "upload request" do
    example "successful request" do
      result =
        handle_iq(shared.user_jid, @server_jid, upload_packet(10000, "all"))

      assert_expected_upload_packet(shared, result)
    end
  end

  describe "message media upload request" do
    example "successful request" do
      result =
        handle_iq(
          shared.user_jid,
          @server_jid,
          upload_packet(10000, shared.bob_access)
        )

      assert_expected_upload_packet(shared, result)
    end
  end

  describe "big upload request" do
    example "oversize request" do
      size = 1024 * 1024 * 10 + 1
      packet = upload_packet(size, "all")
      result = handle_iq(shared.user_jid, @server_jid, packet)

      iq(sub_el: req_sub_el) = packet

      iq(
        type: :error,
        sub_el: [
          ^req_sub_el,
          xmlel(
            name: "error",
            attrs: [{"code", "406"}, {"type", "modify"}],
            children: [
              xmlel(name: "not-acceptable"),
              xmlel(name: "text", children: [xmlcdata: text])
            ]
          )
        ]
      ) = result

      text |> should(match ~r/Invalid size/)
    end
  end

  describe "avatar download request" do
    example "successful request on own avatar using an ID" do
      packet = download_packet(shared.avatar.id)
      result = handle_iq(shared.user_jid, @server_jid, packet)
      assert_expected_download_packet(result)
    end

    example "successful request on own avatar using a URL" do
      url =
        shared.alice.id
        |> TROS.make_jid(shared.server, shared.avatar.id)
        |> TROS.make_url()

      packet = download_packet(url)
      result = handle_iq(shared.user_jid, @server_jid, packet)
      assert_expected_download_packet(result)
    end

    example "successful request on someone else's avatar" do
      user_jid = User.to_jid(shared.bob)
      packet = download_packet(shared.avatar.id)
      result = handle_iq(user_jid, @server_jid, packet)
      assert_expected_download_packet(result)
    end
  end

  describe "messge media download request" do
    example "successful request on own media" do
      packet = download_packet(shared.media.id)
      result = handle_iq(shared.user_jid, @server_jid, packet)
      assert_expected_download_packet(result)
    end

    example "successful request on someone else's media that was sent to us" do
      user_jid = User.to_jid(shared.bob)
      packet = download_packet(shared.media.id)
      result = handle_iq(user_jid, @server_jid, packet)
      assert_expected_download_packet(result)
    end

    example "failed request on someone else's media that was NOT sent to us" do
      user_jid = User.to_jid(shared.carol)
      packet = download_packet(shared.media.id)
      result = handle_iq(user_jid, @server_jid, packet)

      iq(sub_el: req_sub_el) = packet

      iq(
        type: :error,
        sub_el: [
          ^req_sub_el,
          xmlel(
            name: "error",
            attrs: [{"code", "403"}, {"type", "auth"}],
            children: [
              xmlel(name: "forbidden"),
              xmlel(name: "text", children: [xmlcdata: text])
            ]
          )
        ]
      ) = result

      text |> should(match ~r/permission_denied/)
    end
  end

  describe "bad file ID on download request" do
    example "failed due to malformed UUID" do
      bad_uuid =
        String.slice(shared.media.id, 1, String.length(shared.media.id) - 1)

      packet = download_packet(bad_uuid)
      result = handle_iq(shared.user_jid, @server_jid, packet)

      iq(sub_el: req_sub_el) = packet

      iq(
        type: :error,
        sub_el: [
          ^req_sub_el,
          xmlel(
            name: "error",
            attrs: [{"code", "404"}, {"type", "cancel"}],
            children: [
              xmlel(name: "item-not-found"),
              xmlel(name: "text", children: [xmlcdata: text])
            ]
          )
        ]
      ) = result

      text |> should(match ~r/File not found/)
    end

    example "failed due to malformed URL" do
      packet = download_packet("tros:bogus")
      result = handle_iq(shared.user_jid, @server_jid, packet)

      iq(sub_el: req_sub_el) = packet

      iq(
        type: :error,
        sub_el: [
          ^req_sub_el,
          xmlel(
            name: "error",
            attrs: [{"code", "406"}, {"type", "modify"}],
            children: [
              xmlel(name: "not-acceptable"),
              xmlel(name: "text", children: [xmlcdata: text])
            ]
          )
        ]
      ) = result

      text |> should(match ~r/Invalid file URL/)
    end

    example "failed due to missing file metadata" do
      packet = download_packet(ID.new())
      result = handle_iq(shared.user_jid, @server_jid, packet)

      iq(sub_el: req_sub_el) = packet

      iq(
        type: :error,
        sub_el: [
          ^req_sub_el,
          xmlel(
            name: "error",
            attrs: [{"code", "404"}, {"type", "cancel"}],
            children: [
              xmlel(name: "item-not-found"),
              xmlel(name: "text", children: [xmlcdata: text])
            ]
          )
        ]
      ) = result

      text |> should(match ~r/File not found/)
    end
  end

  defp common_packet(type, request),
    do: iq(id: "123456", type: type, sub_el: request)

  defp upload_packet(size, access),
    do: common_packet(:set, upload_request(size, access))

  defp upload_request(size, access) do
    elements = [
      {"filename", @filename},
      {"size", Integer.to_string(size)},
      {"mime-type", "image/jpeg"},
      {"access", access}
    ]

    xmlel(
      name: "upload-request",
      attrs: [{"xmlns", @ns_tros}],
      children: upload_children(elements)
    )
  end

  defp upload_children(elements) do
    for {n, c} <- elements, do: xmlel(name: n, children: [xmlcdata: c])
  end

  defp download_packet(file_id),
    do: common_packet(:get, download_request(file_id))

  defp download_request(file_id) do
    xmlel(
      name: "download-request",
      attrs: [{"xmlns", @ns_tros}],
      children: [
        xmlel(name: "id", children: [xmlcdata: file_id])
      ]
    )
  end

  defp assert_expected_upload_packet(shared, packet) do
    iq(
      type: :result,
      sub_el:
        xmlel(
          name: "upload",
          children: [
            xmlel(name: "headers"),
            xmlel(name: "id", children: [xmlcdata: file_id]),
            xmlel(name: "jid", children: [xmlcdata: jid]),
            xmlel(name: "method", children: [xmlcdata: "PUT"]),
            xmlel(name: "url"),
            xmlel(name: "reference_url", children: [xmlcdata: ref_url])
          ]
        )
    ) = packet

    shared.server
    |> TROS.make_jid(file_id)
    |> JID.to_binary()
    |> should(eq jid)

    shared.alice.id
    |> TROS.make_jid(shared.server, file_id)
    |> TROS.make_url()
    |> should(eq ref_url)
  end

  defp assert_expected_download_packet(packet) do
    assert iq(
             type: :result,
             sub_el:
               xmlel(
                 name: "download",
                 children: [
                   xmlel(name: "headers", children: []),
                   xmlel(name: "url")
                 ]
               )
           ) = packet
  end
end
