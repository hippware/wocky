# @copyright 2016+ Hippware, Inc.
defmodule Wocky.TROS do
  @moduledoc "API module for the TROS file handling system."

  use Wocky.Context

  alias Wocky.Account.User
  alias Wocky.Repo.ID
  alias Wocky.TROS.Metadata

  @type owner :: String.t()
  @type file_id :: String.t()
  @type file_name :: String.t()
  @type url :: String.t()
  @type metadata :: map()
  @type file_type :: Metadata.FileTypeEnum.t()

  @callback delete(file_id()) :: :ok
  @callback make_upload_response(String.t(), file_id(), integer(), metadata()) ::
              {list(), list()}
  @callback get_download_url(metadata(), file_name()) :: url()

  @thumbnail_suffix "-thumbnail"
  @aspect_thumbnail_suffix "-aspect_thumbnail"
  @original_suffix "-original"

  @file_ready_event_prefix "tros-file-ready-"

  @valid_content_types ["image/png", "image/jpeg"]

  @regex ~r/tros:(?:([\w-]+)@)?([\w\.]+)\/file\/([\w-]+)/

  # ----------------------------------------------------------------------
  # Names and URLs

  @spec parse_url(url()) :: {:ok, file_id()} | {:error, :invalid_url}
  def parse_url(url) do
    server = Wocky.host()

    case Regex.run(@regex, url, capture: :all_but_first) do
      [_user_id, ^server, file_id] -> {:ok, file_id}
      _ -> {:error, :invalid_url}
    end
  end

  @spec make_url(User.tid(), file_id()) :: url()
  def make_url(owner, file_id) do
    url_prefix() <> url_userpart(User.id(owner)) <> url_filepart(file_id)
  end

  @spec make_url(file_id()) :: url()
  def make_url(file_id) do
    url_prefix() <> url_filepart(file_id)
  end

  defp url_prefix, do: "tros:"

  defp url_userpart(user_id), do: "#{user_id}@"

  defp url_filepart(file_id), do: "#{Wocky.host()}/file/#{file_id}"

  @spec get_base_id(file_name()) :: file_id()
  def get_base_id(file_name) do
    [@thumbnail_suffix, @aspect_thumbnail_suffix, @original_suffix]
    |> Enum.reduce(file_name, &String.replace_suffix(&2, &1, ""))
  end

  @spec variants(file_id()) :: [String.t()]
  def variants(file_id) do
    Enum.map(
      [:full, :thumbnail, :aspect_thumbnail, :original],
      &full_name(file_id, &1)
    )
  end

  # ----------------------------------------------------------------------
  # Data management

  @spec get_metadata(file_id()) :: {:ok, Metadata.t()} | {:error, any()}
  def get_metadata(id) do
    if ID.valid?(id) do
      case Repo.get(Metadata, id) do
        nil -> {:error, :not_found}
        value -> {:ok, value}
      end
    else
      {:error, :not_found}
    end
  end

  @spec delete(file_id(), User.tid()) :: {:ok, Metadata.t()}
  def delete(file_id, requestor) do
    user_id = User.id(requestor)

    case Repo.get(Metadata, file_id) do
      %Metadata{user_id: ^user_id} = metadata ->
        do_delete(metadata)

      nil ->
        {:error, :not_found}

      _ ->
        {:error, :permission_denied}
    end
  end

  @spec delete_all(User.t()) :: :ok
  def delete_all(user) do
    Repo.transaction(fn ->
      user
      |> assoc(:tros_metadatas)
      |> Repo.stream()
      |> Stream.each(&do_delete/1)
      |> Stream.run()
    end)
  end

  defp do_delete(md) do
    with {:ok, file} <- Repo.delete(md, returning: true) do
      backend().delete(md.id)
      {:ok, file}
    end
  end

  @spec make_upload_response(
          User.t(),
          file_id(),
          integer(),
          String.t(),
          metadata()
        ) :: {:ok, {list(), list()}} | {:error, any()}
  def make_upload_response(owner, file_id, size, access, meta) do
    with true <- meta.content_type in @valid_content_types,
         {:ok, _} <- put_metadata(file_id, owner, access) do
      reference_url = make_url(owner, file_id)

      result =
        backend().make_upload_response(reference_url, file_id, size, meta)

      {:ok, result}
    else
      false ->
        {:error,
         "Invalid MIME type - must be one of #{inspect(@valid_content_types)}"}

      {:error, _} = error ->
        error
    end
  end

  @spec put_metadata(file_id(), User.t(), atom()) :: Repo.result(Metadata.t())
  def put_metadata(id, user, access) do
    user
    |> build_assoc(:tros_metadatas)
    |> Metadata.changeset(%{id: id, access: access, ready: false})
    |> Repo.insert()
  end

  @spec get_download_urls(Metadata.t()) :: %{optional(file_type()) => url()}
  def get_download_urls(metadata) do
    metadata.available_formats
    |> Enum.map(
      &{&1, backend().get_download_url(metadata, full_name(metadata.id, &1))}
    )
    |> Enum.into(%{})
  end

  @spec file_ready_event(file_id()) :: String.t()
  def file_ready_event(file_id), do: @file_ready_event_prefix <> file_id

  defp full_name(file_id, :full), do: file_id
  defp full_name(file_id, :thumbnail), do: file_id <> @thumbnail_suffix

  defp full_name(file_id, :aspect_thumbnail),
    do: file_id <> @aspect_thumbnail_suffix

  defp full_name(file_id, :original), do: file_id <> @original_suffix

  defp backend, do: Confex.get_env(:wocky, :tros_backend)
end
