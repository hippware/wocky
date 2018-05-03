defmodule Wocky.User.Avatar do
  @moduledoc """
  Validation and logic regarding user avatars.
  """

  import OK, only: [~>>: 2]

  alias Wocky.Repo.ID
  alias Wocky.TROS
  alias Wocky.TROS.Metadata
  alias Wocky.User

  @type url :: binary
  @type t :: {User.server(), User.id()}

  @spec validate(User.t(), t()) :: {:ok, t()} | {:error, :atom}
  def validate(user, avatar) do
    prepare(avatar)
    ~>> check_valid_filename()
    ~>> check_is_local(user.server)
    ~>> check_owner(user.id)
  end

  defp prepare(avatar_url), do: TROS.parse_url(avatar_url)

  defp check_is_local({server, _} = avatar, server), do: {:ok, avatar}
  defp check_is_local(_, _), do: {:error, :not_local_file}

  defp check_valid_filename({_server, file_id} = avatar) do
    if ID.valid?(file_id) do
      {:ok, avatar}
    else
      {:error, :invalid_file}
    end
  end

  defp check_owner({_server, id} = avatar, user_id) do
    case TROS.get_metadata(id) do
      {:ok, %Metadata{user_id: ^user_id}} -> {:ok, avatar}
      {:ok, _} -> {:error, :not_file_owner}
      error -> error
    end
  end

  @spec maybe_delete_existing(url | nil, url | nil) :: :ok

  # For this call, a `nil` in the new position means that the changeset
  # for the user is not changing the avatar, not that it is setting it
  # to be empty. Therefore we take no action in that case:
  def maybe_delete_existing(nil, _old_avatar), do: :ok
  def maybe_delete_existing(_new_avatar, nil), do: :ok
  def maybe_delete_existing(avatar, avatar), do: :ok

  def maybe_delete_existing(_new_avatar, old_avatar) do
    case TROS.parse_url(old_avatar) do
      {:ok, {file_server, file_id}} ->
        TROS.delete(file_server, file_id)

      {:error, _} ->
        :ok
    end

    :ok
  end
end
