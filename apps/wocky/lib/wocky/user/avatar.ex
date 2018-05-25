defmodule Wocky.User.Avatar do
  @moduledoc """
  Validation and logic regarding user avatars.
  """

  alias Wocky.Repo.ID
  alias Wocky.TROS
  alias Wocky.TROS.Metadata
  alias Wocky.User

  @type url :: binary
  @type t :: {User.server(), User.id()}

  @spec validate(User.t(), url()) :: {:ok, t()} | {:error, atom()}
  def validate(user, avatar_url) do
    with {:ok, avatar} <- prepare(avatar_url),
         :ok <- check_is_local(avatar, user.server),
         :ok <- check_valid_filename(avatar),
         :ok <- check_owner(avatar, user.id) do
      {:ok, avatar}
    end
  end

  defp prepare(avatar_url), do: TROS.parse_url(avatar_url)

  defp check_is_local({server, _} = _avatar, server), do: :ok
  defp check_is_local(_, _), do: {:error, :not_local_file}

  defp check_valid_filename({_server, file_id} = _avatar) do
    if ID.valid?(file_id) do
      :ok
    else
      {:error, :invalid_file}
    end
  end

  defp check_owner({_server, id} = _avatar, user_id) do
    case TROS.get_metadata(id) do
      {:ok, %Metadata{user_id: ^user_id}} -> :ok
      {:ok, _} -> {:error, :not_file_owner}
      error -> error
    end
  end

  @spec maybe_delete_existing(url | nil, User.t()) :: :ok

  # For this call, a `nil` in the new position means that the changeset
  # for the user is not changing the avatar, not that it is setting it
  # to be empty. Therefore we take no action in that case:
  def maybe_delete_existing(nil, %User{avatar: _old_avatar}), do: :ok
  def maybe_delete_existing(_new_avatar, %User{avatar: nil}), do: :ok
  def maybe_delete_existing(avatar, %User{avatar: avatar}), do: :ok

  def maybe_delete_existing(_new_avatar, user) do
    case TROS.parse_url(user.avatar) do
      {:ok, {_file_server, file_id}} ->
        TROS.delete(file_id, user)

      {:error, _} ->
        :ok
    end

    :ok
  end
end
