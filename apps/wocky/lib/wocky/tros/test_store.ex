defmodule Wocky.TROS.TestStore do
  @moduledoc "Test backend for the TROS file management system"

  use Wocky.JID

  alias Wocky.TROS

  @behaviour TROS

  def delete(_file_id) do
    :ok
  end

  def make_download_response(_file_name) do
    resp_fields = [
      {"url", "http://localhost/some/file/location"}
    ]

    {[], resp_fields}
  end

  def make_upload_response(owner_jid, file_id, _size, _metadata) do
    jid(luser: owner) = owner_jid
    file_jid = TROS.make_jid(owner, file_id)
    reference_url = TROS.make_url(file_jid)
    url = "http://localhost/some/file/location"

    resp_fields = resp_fields(:put, url, reference_url)

    {[], resp_fields}
  end

  def get_download_url(_metadata, file_name) do
    "https://" <> Wocky.host() <> "/" <> file_name
  end

  defp resp_fields(method, url, reference_url) do
    [
      {"method", method |> to_string |> String.upcase()},
      {"url", url},
      {"reference_url", reference_url}
    ]
  end
end
