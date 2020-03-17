defmodule Wocky.TROS.Store.Simple do
  @moduledoc """
  Simple backend for the TROS file management system using
  https://hub.docker.com/r/mayth/simple-upload-server/
  """

  @behaviour TROS

  alias Wocky.Repo
  alias Wocky.TROS
  alias Wocky.TROS.Metadata

  @impl true
  def delete(_file_id) do
    :ok
  end

  @impl true
  def make_upload_response(reference_url, file_id, _size, _metadata) do
    resp_fields = resp_fields(:put, url(file_id), reference_url)

    # No S3 callbacks to set the file ready, so just assume it is
    set_ready(file_id)

    {[], resp_fields}
  end

  defp set_ready(id) do
    Metadata
    |> Repo.get!(id)
    |> Metadata.changeset(%{ready: true})
    |> Repo.update!()
  end

  @impl true
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

  @spec url(TROS.file_id()) :: String.t()
  def url(file_id),
    do: "http://localhost:4569/files/#{Wocky.host()}-#{file_id}?token=1234"
end
