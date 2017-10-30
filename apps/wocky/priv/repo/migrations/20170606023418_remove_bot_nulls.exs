defmodule Wocky.Repo.Migrations.RemoveBotNulls do
  use Wocky.Repo.Migration

  import Ecto.Query

  alias Wocky.Repo

  def change do

    from(b in "bots",
      update: [set: [title: ""]],
      where: is_nil b.title)
    |> Repo.update_all([])

    from(b in "bots",
      update: [set: [address: ""]],
      where: is_nil b.address)
    |> Repo.update_all([])

    from(b in "bots",
      update: [set: [type: ""]],
      where: is_nil b.type)
    |> Repo.update_all([])

    from(b in "bots",
      update: [set: [description: ""]],
      where: is_nil b.description)
    |> Repo.update_all([])

    flush()

    alter table(:bots) do
      modify :title,       :string, null: false, default: ""
      modify :address,     :string, null: false, default: ""
      modify :type,        :string, null: false, default: ""
      modify :description, :text,   null: false, default: ""
    end
  end

end
