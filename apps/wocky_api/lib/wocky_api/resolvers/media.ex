# credo:disable-for-this-file Credo.Check.Readability.Specs
defmodule WockyAPI.Resolvers.Media do
  @moduledoc "GraphQL resolver for media objects"

  use Elixometer

  alias Wocky.Repo.ID
  alias Wocky.TROS
  alias Wocky.TROS.Metadata
  alias Wocky.Waiter

  @ready_timeout 10_000

  # -------------------------------------------------------------------
  # Queries

  def get_media_urls(args, %{context: %{current_user: _user}}) do
    get_urls(args[:tros_url], true, args[:timeout] || @ready_timeout)
  end

  def get_media(%{image_url: tros_url}, _args, _info), do: get_urls(tros_url)

  defp get_urls(tros_url, wait? \\ false, timeout \\ nil)

  defp get_urls(nil, _, _), do: {:ok, nil}

  defp get_urls(tros_url, wait?, timeout) do
    with {:ok, file_id} <- TROS.parse_url(tros_url),
         :ok <- maybe_wait(wait?, file_id, timeout),
         {:ok, %Metadata{} = metadata} <- TROS.get_metadata(file_id) do
      urls =
        metadata
        |> TROS.get_download_urls()
        |> Map.delete(:original)

      {:ok,
       %{
         tros_url: tros_url,
         full_url: urls[:full],
         thumbnail_url: urls[:thumbnail],
         urls: Enum.map(urls, &make_url/1)
       }}
    else
      :timeout ->
        {:error, "Timeout waiting for file to become ready"}

      _ ->
        {:ok,
         %{
           tros_url: tros_url,
           urls: []
         }}
    end
  end

  defp maybe_wait(false, _, _), do: :ok

  defp maybe_wait(true, file_id, timeout) do
    file_id
    |> TROS.file_ready_event()
    |> Waiter.wait(timeout, fn -> file_ready(file_id) end)
  end

  defp file_ready(file_id) do
    case TROS.get_metadata(file_id) do
      {:ok, %Metadata{ready: true}} -> true
      _ -> false
    end
  end

  defp make_url({type, url}), do: %{type: type, url: url}

  # -------------------------------------------------------------------
  # Mutations

  def media_upload(%{input: args}, %{context: %{current_user: user}}) do
    metadata = %{content_type: args[:mime_type], name: args[:filename]}
    id = ID.new()

    response =
      TROS.make_upload_response(
        user,
        id,
        args[:size],
        args[:access] || "",
        metadata
      )

    case response do
      {:ok, {headers, fields}} ->
        update_counter("tros.upload.requests.graphql", 1)

        {:ok,
         %{
           id: id,
           upload_url: :proplists.get_value("url", fields),
           method: :proplists.get_value("method", fields),
           headers: make_return_headers(headers),
           reference_url: :proplists.get_value("reference_url", fields)
         }}

      {:error, e} ->
        {:error, e}
    end
  end

  defp make_return_headers(headers),
    do: Enum.map(headers, fn {n, v} -> %{name: n, value: v} end)

  def media_delete(args, %{context: %{current_user: user}}) do
    with {:ok, file_id} <- TROS.parse_url(args[:input][:url]),
         {:ok, _file} <- TROS.delete(file_id, user) do
      {:ok, true}
    else
      {:error, :invalid_url} -> {:error, "Invalid URL"}
      {:error, :permission_denied} -> {:error, "Permission denied"}
      {:error, :not_found} -> {:error, "File not found"}
    end
  end
end
