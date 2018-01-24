defmodule Wocky.User.HSPrepop do
  @moduledoc """
  Module for dealing with the homestream prepopulation archive user.
  """

  alias Wocky.InitialContact
  alias Wocky.Repo
  alias Wocky.RosterItem
  alias Wocky.User

  @spec handle :: User.handle() | nil
  def handle, do: Confex.get_env(:wocky, :hs_prepopulation_user)

  @spec user :: User.t() | nil
  def user, do: Repo.get_by(User, handle: handle() || "")

  @spec add_source(User.handle()) :: :ok
  def add_source(handle) do
    user = Repo.get_by(User, handle: handle)
    RosterItem.follow(user().id, user.id)
    InitialContact.put(user, :followee)
  end
end
