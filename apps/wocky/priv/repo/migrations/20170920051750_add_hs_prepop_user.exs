defmodule Wocky.Repo.Migrations.AddHsPrepopUser do
  use Wocky.Repo.Migration

  import Ecto.Query

  alias Wocky.Repo
  alias Wocky.Repo.ID
  alias Wocky.Roster.Item, as: RosterItem
  alias Wocky.Roster.InitialContact
  alias Wocky.User

  @handle "__new_user_hs_archive__"

  def up do
    id = ID.new
    execute """
    INSERT INTO users (id, username, server, external_id, handle, created_at, updated_at) VALUES
    ('#{id}', '#{id}', '', '', '#{@handle}', now(), now());
    """

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
