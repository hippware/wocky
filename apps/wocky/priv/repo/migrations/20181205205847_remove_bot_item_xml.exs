defmodule Wocky.Repo.Migrations.RemoveBotItemXml do
  use Wocky.Repo.Migration

  import Ecto.Query
  import SweetXml

  alias Ecto.Changeset
  alias Wocky.Repo

  defmodule BotItem do
    use Wocky.Repo.Schema

    @foreign_key_type :binary_id
    @primary_key {:id, :binary_id, autogenerate: false}
    schema "bot_items" do
      field :stanza, :string
      field :image, :boolean, default: false
      field :content, :string
      field :image_url, :string
    end
  end

  def up do
    alter table(:bot_items) do
      add :content, :text
      add :image_url, :string
    end

    flush()

    from(i in BotItem)
    |> Repo.stream()
    |> Stream.map(&migrate_item/1)
    |> Stream.run()

    alter table(:bot_items) do
      remove :stanza
      remove :image
    end
  end

  defp migrate_item(%BotItem{} = item) do
    content = xpath(item.stanza, ~x"//entry/content/text()"S)
    image_url = xpath(item.stanza, ~x"//entry/image/text()"S)

    item
    |> Changeset.change(content: content)
    |> Changeset.change(image_url: image_url)
    |> Repo.update()
  end
end
