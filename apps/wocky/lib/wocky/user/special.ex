defmodule Wocky.User.Special do
  @moduledoc """
  Module for dealing with special case users such as the homestream
  prepopulation archive user.
  """

  alias Wocky.InitialContact
  alias Wocky.Repo
  alias Wocky.RosterItem
  alias Wocky.User

  @spec hs_prepopulation_handle :: User.handle | nil
  def hs_prepopulation_handle,
    do: Confex.get_env(:wocky, :hs_prepopulation_user)

  @spec hs_prepopulation_user :: User.t | nil
  def hs_prepopulation_user,
    do: Repo.get_by(User, handle: hs_prepopulation_handle() || "")

  @spec add_hs_prepop_source(User.handle) :: :ok
  def add_hs_prepop_source(handle) do
    user = Repo.get_by(User, handle: handle)
    RosterItem.follow(hs_prepopulation_user().id, user.id)
    InitialContact.put(user, :followee)
  end
end
