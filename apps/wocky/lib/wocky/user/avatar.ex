defmodule Wocky.User.Avatar do
  @moduledoc """
  Validation and logic regarding user avatars.
  """

  alias Wocky.TROS
  alias Wocky.User

  @type url :: binary
  @type t :: {User.server, User.id}

  @spec prepare(url) :: {:ok, t} | {:error, any}
  def prepare(avatar_url), do: TROS.parse_url(avatar_url)

  @spec check_is_local(t, User.server) :: {:ok, t} | {:error, any}
  def check_is_local({server, _} = avatar, server), do: {:ok, avatar}
  def check_is_local(_, _), do: {:error, :not_local_file}

  @spec check_owner(t, User.id) :: {:ok, t} | {:error, any}
  def check_owner({_server, id} = avatar, user_id) do
    case TROS.get_owner(id) do
      {:ok, ^user_id} -> {:ok, avatar}
      {:ok, _} -> {:error, :not_file_owner}
      error -> error
    end
  end

  @spec delete_existing(t, url) :: {:ok, t} | {:error, any}
  def delete_existing(new_avatar, nil), do: {:ok, new_avatar}
  def delete_existing(new_avatar, old_avatar) do
    case TROS.parse_url(old_avatar) do
      {:ok, {file_server, file_id}} ->
        TROS.delete(file_server, file_id)

      {:error, _} -> :ok
    end

    {:ok, new_avatar}
  end

  @spec to_url(t) :: url
  def to_url({server, file_id}), do: {:ok, TROS.make_url(server, file_id)}
end
