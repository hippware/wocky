# @copyright 2016+ Hippware, Inc.
defmodule Wocky.TROS do
  @moduledoc "API module for the TROS file handling system."

  use Wocky.JID

  alias Wocky.Repo.ID
  alias Wocky.TROS.Metadata

  @type owner :: JID.luser()
  @type server :: JID.lserver()
  @type file_id :: JID.lresource()
  @type file_name :: binary
  @type url :: binary
  # %{binary => binary}
  @type metadata :: map
  @type file_type :: :full | :original | :thumbnail

  @type error :: :not_found | :metadata_not_found | {:retrieve_error, binary}
  @type result(type) :: {:ok, type} | {:error, error}

  @callback delete(server, file_id) :: :ok
  @callback make_upload_response(JID.t(), file_id, integer, metadata) ::
              {list, list}
  @callback make_download_response(server, file_name) :: {list, list}
  @callback get_download_url(server, metadata, file_name) :: url

  @thumbnail_suffix "-thumbnail"
  @original_suffix "-original"

  @spec make_jid(server, file_id) :: JID.t()
  def make_jid(server, file_id), do: make_jid("", server, file_id)

  @spec make_jid(owner, server, file_id) :: JID.t()
  def make_jid(owner, server, file_id),
    do: JID.make(owner, server, "file/#{file_id}")

  @spec parse_url(url) :: {:ok, {server, file_id}} | {:error, :invalid_url}
  def parse_url("tros:" <> jid) do
    jid(lserver: server, lresource: resource) = JID.from_binary(jid)

    case resource do
      "file/" <> file_id -> {:ok, {server, file_id}}
      _ -> {:error, :invalid_url}
    end
  end

  def parse_url(_), do: {:error, :invalid_url}

  @spec make_url(server, file_id) :: url
  def make_url(server, file_id), do: server |> make_jid(file_id) |> make_url

  @spec make_url(JID.t()) :: url
  def make_url(jid), do: "tros:#{JID.to_binary(jid)}"

  @spec get_base_id(file_name) :: file_id
  def get_base_id(file_name) do
    file_name
    |> String.replace_suffix(@thumbnail_suffix, "")
    |> String.replace_suffix(@original_suffix, "")
  end

  @spec get_type(file_id) :: file_type
  def get_type(file_id) do
    cond do
      String.ends_with?(file_id, @original_suffix) -> :original
      String.ends_with?(file_id, @thumbnail_suffix) -> :thumbnail
      true -> :full
    end
  end

  @spec variants(file_id) :: [binary]
  def variants(file_id) do
    Enum.map([:full, :thumbnail, :original], &full_name(file_id, &1))
  end

  @spec get_metadata(file_id) :: result(Metadata.t())
  def get_metadata(id) do
    if ID.valid?(id) do
      case Metadata.get(id) do
        nil -> {:error, :not_found}
        value -> {:ok, value}
      end
    else
      {:error, :not_found}
    end
  end

  @spec update_access(file_id, binary) :: result(Metadata.t())
  def update_access(file_id, new_access) do
    Metadata.set_access(file_id, new_access)
  end

  @spec delete(server, file_id, User.t()) :: :ok
  def delete(server, file_id, requestor) do
    with :ok <- Metadata.delete(file_id, requestor) do
      backend().delete(server, file_id)
    end
  end

  @spec make_upload_response(JID.t(), file_id, integer, binary, metadata) ::
          {:ok, {list, list}} | {:error, term}
  def make_upload_response(owner_jid, file_id, size, access, meta) do
    jid(luser: owner_id) = owner_jid

    case Metadata.put(file_id, owner_id, access) do
      {:ok, _} ->
        {:ok, backend().make_upload_response(owner_jid, file_id, size, meta)}

      {:error, _} = error ->
        error
    end
  end

  @spec make_download_response(server, file_id) :: {:ok, {list, list}}
  def make_download_response(server, file_id),
    do: {:ok, backend().make_download_response(server, file_id)}

  @spec ready?(file_id) :: boolean
  def ready?(file_id), do: Metadata.ready?(file_id)

  @spec get_download_urls(server, Metadata.t(), [file_type]) :: [url]
  def get_download_urls(server, metadata, types) do
    Enum.map(
      types,
      &backend().get_download_url(server, metadata, full_name(metadata.id, &1))
    )
  end

  defp full_name(file_id, :full), do: file_id
  defp full_name(file_id, :thumbnail), do: file_id <> @thumbnail_suffix
  defp full_name(file_id, :original), do: file_id <> @original_suffix

  defp backend do
    Confex.get_env(:wocky, :tros_backend)
  end
end
