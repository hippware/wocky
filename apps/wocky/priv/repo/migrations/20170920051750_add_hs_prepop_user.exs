defmodule Wocky.Repo.Migrations.AddHsPrepopUser do
  use Wocky.Repo.Migration

  import Ecto.Query

  alias Wocky.InitialContact
  alias Wocky.Repo
  alias Wocky.Repo.ID
  alias Wocky.RosterItem
  alias Wocky.User

  @handle "__new_user_hs_archive__"

  def up do
    id = ID.new
    %User{
      id: id,
      username: id,
      server: "",
      external_id: "",
      handle: @handle
    }
    |> Repo.insert

    InitialContact
    |> where([i], i.type == "friend" or i.type == "followee")
    |> Repo.all
    |> Enum.each(&follow(id, &1.user_id))
  end

  def down do
    User
    |> where([u], handle: @handle)
    |> Repo.delete_all
  end

  defp follow(user, target) do
    %RosterItem{
      user_id: user,
      contact_id: target,
      subscription: :to,
      ask: :none,
      groups: []
    }
    |> Repo.insert

    %RosterItem{
      user_id: target,
      contact_id: user,
      subscription: :from,
      ask: :none,
      groups: []
    }
    |> Repo.insert
  end
end
