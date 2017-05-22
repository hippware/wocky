defmodule Wocky.TROS.S3Store do
  @moduledoc "S3 backend for the TROS file management system"

  use Wocky.JID

  alias ExAws.Auth
  alias ExAws.Config
  alias ExAws.S3
  alias Wocky.TROS

  @behaviour TROS

  @amz_content_type "content-type"
  @link_expiry 60 * 10 # 10 minute expiry on upload/download links.

  def delete(lserver, file_id) do
    for file <- TROS.variants(file_id) do
      do_delete(lserver, file)
    end
    :ok
  end

  def do_delete(lserver, file_id) do
    with {:ok, result} <- do_request(lserver, file_id, :delete) do
      check_result_get_headers(result, 204)
    else
      {:error, _} = error -> error
    end
  end

  defp do_request(lserver, file_id, type) do
    url =
      lserver
      |> s3_url(bucket(), file_id, type, [])
      |> String.to_charlist

    case :httpc.request(type, {url, []}, [], []) do
      {:ok, _} = res -> res
      {:error, err} -> {:error, {:retrieve_error, "Error: #{inspect(err)}"}}
    end
  end

  defp check_result_get_headers({{_, expected, _}, headers, _body}, expected) do
    {:ok, headers}
  end
  defp check_result_get_headers({{_, code, _}, _headers, _body}, _expected)
  when code == 404 or code == 403 do
    # S3 likes to return 403 for non-existant files
    {:error, :not_found}
  end
  defp check_result_get_headers({error, headers, body}, expected) do
    text = """
    Error performing operation (expected #{expected}): \
    #{inspect(error)} #{inspect(headers)} #{inspect(body)}\
    """
    {:error, {:retrieve_error, text}}
  end

  def make_download_response(server, file_id) do
    resp_fields = [
      {"url", s3_url(server, bucket(), file_id, :get, [])}
    ]

    {[], resp_fields}
  end

  def make_upload_response(owner_jid, file_id, size, metadata) do
    jid(luser: owner, lserver: lserver) = owner_jid
    file_jid = TROS.make_jid(owner, lserver, file_id)
    reference_url = TROS.make_url(file_jid)

    headers = [
     {"x-amz-content-sha256", "UNSIGNED-PAYLOAD"},
     {"content-length", Integer.to_string(size)},
     {@amz_content_type, Map.get(metadata, @amz_content_type)}
    ]

    config = Config.new(:s3, [access_key_id: access_key_id(),
                              secret_access_key: secret_key()])

    url =
      "https://#{upload_bucket()}.s3.amazonaws.com/#{path(lserver, file_id)}"

    {:ok, ret_headers} = Auth.headers(:put, url, :s3, config, headers, nil)

    resp_fields = resp_fields(:put, url, reference_url)

    {ret_headers, resp_fields}
  end

  defp resp_fields(method, url, reference_url) do
    [
      {"method", method |> to_string |> String.upcase},
      {"url", url},
      {"reference_url", reference_url}
    ]
  end

  defp s3_url(server, bucket, file_id, method, url_params) do
    config = Config.new(:s3, [access_key_id: access_key_id(),
                              secret_access_key: secret_key()])

    options = [
      expires_in: @link_expiry,
      virtual_host: false,
      query_params: url_params
    ]

    {:ok, url} = S3.presigned_url(config, method, bucket,
                                  path(server, file_id), options)

    url
  end

  def upload_bucket, do: "#{bucket()}-quarantine"

  def bucket, do: get_opt(:tros_s3_bucket)

  def access_key_id, do: get_opt(:tros_s3_access_key_id)

  def secret_key, do: get_opt(:tros_s3_secret_key)

  defp get_opt(opt), do: Confex.get(:wocky, opt)

  defp path(server, file_id),
    do: "#{server}-#{hash_prefix(file_id)}/#{file_id}"

  defp hash_prefix(file_id) do
    file_id
    |> TROS.get_base_id
    |> do_hash()
    |> :binary.part(0, 2)
    |> Base.encode16
  end

  defp do_hash(str), do: :crypto.hash(:md5, str)
end
