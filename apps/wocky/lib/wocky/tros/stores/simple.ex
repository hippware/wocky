defmodule Wocky.TROS.Store.Simple do
  @moduledoc """
  Simple backend for the TROS file management system using
  https://hub.docker.com/r/mayth/simple-upload-server/
  """

  use Wocky.JID

  alias Wocky.TROS
  alias Wocky.TROS.Metadata

  @behaviour TROS

  def delete(_file_id) do
    :ok
  end

  def make_download_response(file_name) do
    resp_fields = [
      {"url", url(TROS.get_base_id(file_name))}
    ]

    {[], resp_fields}
  end

  def make_upload_response(owner_jid, file_id, _size, _metadata) do
    jid(luser: owner) = owner_jid
    file_jid = TROS.make_jid(owner, file_id)
    reference_url = TROS.make_url(file_jid)

    resp_fields = resp_fields(:put, url(file_id), reference_url)

    # No S3 callbacks to set the file ready, so just assume it is
    Metadata.set_ready(file_id)

    {[], resp_fields}
  end

  def get_download_url(_metadata, file_name) do
    url(TROS.get_base_id(file_name))
  end

  defp resp_fields(method, url, reference_url) do
    [
      {"method", method |> to_string |> String.upcase()},
      {"url", url},
      {"reference_url", reference_url}
    ]
  end

  def url(file_id),
    do: "http://localhost:4569/files/#{Wocky.host()}-#{file_id}?token=1234"
end
