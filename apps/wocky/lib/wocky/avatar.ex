defmodule Wocky.User.Avatar do
  @moduledoc """
  Validation and logic regarding user avatars.
  """

  import OK, only: ["~>>": 2]

  alias Wocky.User

  @type url :: binary
  @type t :: {User.server, User.id}

  @spec prepare(url) :: {:ok, t} | {:error, any}
  def prepare(avatar_url), do: :tros.parse_url(avatar_url)

  @spec check_is_local(t, User.server) :: {:ok, t} | {:error, any}
  def check_is_local({server, _} = avatar, server), do: {:ok, avatar}
  def check_is_local(_, _), do: {:error, :not_local_file}

  @spec check_owner(t, User.id) :: {:ok, t} | {:error, any}
  def check_owner({server, id} = avatar, user_id) do
    case :tros.get_metadata(server, id) ~>> :tros.get_owner do
      {:ok, ^user_id} -> {:ok, avatar}
      {:ok, _} -> {:error, :not_file_owner}
      error -> error
    end
  end

  @spec delete_existing(t, url) :: {:ok, t} | {:error, any}
  def delete_existing(new_avatar, nil), do: {:ok, new_avatar}
  def delete_existing(new_avatar, old_avatar) do
    case :tros.parse_url(old_avatar) do
      {:ok, {file_server, file_id}} ->
        :tros.delete(file_server, file_id)

      {:error, _} -> :ok
    end

    {:ok, new_avatar}
  end

  @spec to_url(t) :: url
  def to_url({server, file_id}), do: {:ok, :tros.make_url(server, file_id)}
end
