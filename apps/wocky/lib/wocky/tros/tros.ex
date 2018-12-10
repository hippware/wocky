# @copyright 2016+ Hippware, Inc.
defmodule Wocky.TROS do
  @moduledoc "API module for the TROS file handling system."

  use Wocky.JID

  alias Wocky.Repo.ID
  alias Wocky.TROS.Metadata
  alias Wocky.User

  @type owner :: binary
  @type file_id :: binary
  @type file_name :: binary
  @type url :: binary
  # %{binary => binary}
  @type metadata :: map
  @type file_type :: :full | :original | :thumbnail

  @type error :: :not_found | :metadata_not_found | {:retrieve_error, binary}
  @type result(type) :: {:ok, type} | {:error, error}

  @callback delete(file_id) :: :ok
  @callback make_upload_response(User.t(), file_id, integer, metadata) ::
              {list, list}
  @callback make_download_response(file_name) :: {list, list}
  @callback get_download_url(metadata, file_name) :: url

  @thumbnail_suffix "-thumbnail"
  @original_suffix "-original"

  @spec parse_url(url) :: {:ok, file_id} | {:error, :invalid_url}
  def parse_url("tros:" <> jid) do
    jid(lserver: url_server, lresource: resource) = JID.from_binary(jid)
    server = Wocky.host()

    case {resource, url_server} do
      {"file/" <> file_id, ^server} -> {:ok, file_id}
      _ -> {:error, :invalid_url}
    end
  end

  def parse_url(_), do: {:error, :invalid_url}

  @spec make_url(User.t(), file_id) :: url
  def make_url(owner, file_id),
    do: file_id |> make_jid(owner.id) |> url_from_jid()

  @spec make_url(file_id) :: url
  def make_url(file_id), do: file_id |> make_jid("") |> url_from_jid()

  defp make_jid(file_id, owner_id),
    do: JID.make(owner_id, Wocky.host(), "file/#{file_id}")

  defp url_from_jid(jid), do: "tros:#{JID.to_binary(jid)}"

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

  @spec delete(file_id, User.t()) :: {:ok, Metadata.t()}
  def delete(file_id, requestor) do
    with {:ok, file} <- Metadata.delete(file_id, requestor) do
      backend().delete(file_id)
      {:ok, file}
    end
  end

  @spec make_upload_response(User.t(), file_id, integer, binary, metadata) ::
          {:ok, {list, list}} | {:error, term}
  def make_upload_response(owner, file_id, size, access, meta) do
    case Metadata.put(file_id, owner.id, access) do
      {:ok, _} ->
        reference_url = make_url(owner, file_id)

        {:ok,
         backend().make_upload_response(reference_url, file_id, size, meta)}

      {:error, _} = error ->
        error
    end
  end

  @spec make_download_response(file_id) :: {:ok, {list, list}}
  def make_download_response(file_id),
    do: {:ok, backend().make_download_response(file_id)}

  @spec ready?(file_id) :: boolean
  def ready?(file_id), do: Metadata.ready?(file_id)

  @spec get_download_urls(Metadata.t(), [file_type]) :: [url]
  def get_download_urls(metadata, types) do
    Enum.map(
      types,
      &backend().get_download_url(metadata, full_name(metadata.id, &1))
    )
  end

  defp full_name(file_id, :full), do: file_id
  defp full_name(file_id, :thumbnail), do: file_id <> @thumbnail_suffix
  defp full_name(file_id, :original), do: file_id <> @original_suffix

  defp backend do
    Confex.get_env(:wocky, :tros_backend)
  end
end
