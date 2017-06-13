defmodule Wocky.Repo.Migrations.StoreTokensAsHashes do
  use Ecto.Migration

  import Ecto.Query

  alias Wocky.Repo
  alias Comeonin.Bcrypt

  def up do
    rename table(:tokens), :token, to: :token_hash

    flush()

    from(t in "tokens",
      select: %{:user_id => t.user_id,
                :resource => t.resource,
                :token_hash => t.token_hash})
    |> Repo.all
    |> Enum.map(&hash_token/1)
    |> Enum.map(&update_token/1)
  end

  def down do
    rename table(:tokens), :token_hash, to: :token

    # Can't recover tokens from the hash
    Repo.delete_all("tokens")
  end

  defp hash_token(map) do
    Map.update(map, :token_hash, "", &Bcrypt.hashpwsalt/1)
  end

  defp update_token(map) do
    from(t in "tokens",
         update: [set: [token_hash: ^map.token_hash]],
         where: [user_id: ^map.user_id, resource: ^map.resource])
    |> Repo.update_all([])
  end
end
