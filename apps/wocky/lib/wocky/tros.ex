# @copyright 2016+ Hippware, Inc.
defmodule Wocky.TROS do
  @moduledoc "API module for the TROS file handling system."

  use Wocky.JID

  alias Wocky.Repo.ID
  alias Wocky.TROS.Metadata

  @type owner     :: JID.luser
  @type server    :: JID.lserver
  @type file_id   :: JID.lresource
  @type url       :: binary # <<_:40,_:_*8>>
  @type metadata  :: map # %{binary => binary}
  @type file_type :: :full | :original | :thumbnail

  @type error :: :not_found | :metadata_not_found | {:retrieve_error, binary}
  @type result(type) :: {:ok, type} | {:error, error}

  @callback delete(server, file_id) :: :ok
  @callback make_upload_response(JID.t, file_id, integer, metadata) ::
    {list, list}
  @callback make_download_response(server, file_id) :: {list, list}

  @thumbnail_suffix "-thumbnail"
  @original_suffix "-original"

  @spec parse_url(url) :: {:ok, {server, file_id}} | {:error, :invalid_url}
  def parse_url("tros:" <> jid) do
    jid(lserver: server, lresource: resource) = JID.from_binary(jid)
    case resource do
      "file/" <> file_id -> {:ok, {server, file_id}}
      _ -> {:error, :invalid_url}
    end
  end
  def parse_url(_), do: {:error, :invalid_url}

  @spec make_jid(server, file_id) :: JID.t
  def make_jid(server, file_id),
    do: make_jid("", server, file_id)

  @spec make_jid(owner, server, file_id) :: JID.t
  def make_jid(owner, server, file_id),
    do: JID.make(owner, server, "file/#{file_id}")

  @spec make_url(server, file_id) :: url
  def make_url(server, file_id),
    do: server |> make_jid(file_id) |> make_url

  @spec make_url(JID.t) :: url
  def make_url(jid),
    do: "tros:#{JID.to_binary(jid)}"

  @spec get_base_id(file_id) :: file_id
  def get_base_id(file_id) do
    file_id
    |> String.replace_suffix(@thumbnail_suffix, "")
    |> String.replace_suffix(@original_suffix, "")
  end

  @spec get_type(file_id) :: file_type
  def get_type(file_id) do
    if String.ends_with?(file_id, @original_suffix) do
      :original
    else
      if String.ends_with?(file_id, @thumbnail_suffix) do
        :thumbnail
      else
        :full
      end
    end
  end

  @spec variants(file_id) :: [binary]
  def variants(file_id) do
    for suffix <- ["", @original_suffix, @thumbnail_suffix] do
      file_id <> suffix
    end
  end

  @spec get_owner(file_id) :: result(owner)
  def get_owner(id), do: get_info(id, &Metadata.get_user_id/1)

  @spec get_access(file_id) :: result(binary)
  def get_access(id), do: get_info(id, &Metadata.get_access/1)

  defp get_info(id, fun) do
    if ID.valid?(id) do
      case fun.(id) do
        nil -> {:error, :not_found}
        value -> {:ok, value}
      end
    else
      {:error, :not_found}
    end
  end

  @spec update_access(file_id, binary) :: result(Metadata.t)
  def update_access(file_id, new_access) do
    Metadata.set_access(file_id, new_access)
  end

  @spec delete(server, file_id) :: :ok
  def delete(server, file_id) do
    Metadata.delete(file_id)
    (backend()).delete(server, file_id)
  end

  @spec make_upload_response(JID.t, file_id, integer, binary, metadata) ::
    {list, list}
  def make_upload_response(owner_jid, file_id, size, access, meta) do
    jid(luser: owner) = owner_jid
    Metadata.put(file_id, owner, access)

    (backend()).make_upload_response(owner_jid, file_id, size, meta)
  end

  @spec make_download_response(server, file_id) :: {list, list}
  def make_download_response(server, file_id),
    do: (backend()).make_download_response(server, file_id)

  defp backend do
    Application.fetch_env!(:wocky, :tros_backend)
  end
end
