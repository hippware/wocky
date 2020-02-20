defmodule Wocky.Repo.Migrations.FixNotifShareType do
  use Wocky.Repo.Migration

  import Ecto.Changeset
  import Ecto.Query

  alias Wocky.Notifier.InBand.Notification
  alias Wocky.Repo

  def up do
    Notification
    |> where([n], n.type == "user_invitation" or n.type == "location_share")
    |> Repo.all()
    |> Enum.each(&fix_share_type/1)
  end

  defp fix_share_type(%Notification{type: :user_invitation} = n) do
    share_type = n.data["share_type"] || "always"

    n.data
    |> Map.put("share_type", share_type)
    |> changeset(n)
    |> Repo.update()
  end

  defp fix_share_type(%Notification{type: :location_share} = n) do
    share_type = n.data["share_type"] || "always"
    other_user_share_type = n.data["other_user_share_type"] || "always"

    n.data
    |> Map.put("share_type", share_type)
    |> Map.put("other_user_share_type", other_user_share_type)
    |> changeset(n)
    |> Repo.update()
  end

  defp changeset(data, struct) do
    cast(struct, %{data: data}, [:data])
  end
end
