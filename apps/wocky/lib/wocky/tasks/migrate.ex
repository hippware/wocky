defmodule Wocky.Tasks.Migrate do
  @moduledoc "Run migrations during deployment"

  alias Ecto.Migrator
  alias Wocky.Repo

  def run do
    Enum.each([:postgrex, :ecto], &Application.ensure_all_started/1)
    Repo.start_link(pool_size: 1)

    Migrator.run(Repo, migrations_path(:wocky), :up, all: true)

    seed_script = seeds_path(:wocky)

    if File.exists?(seed_script) do
      Code.eval_file(seed_script)
    end
  end

  defp priv_dir(app), do: "#{:code.priv_dir(app)}"

  defp migrations_path(app),
    do: Path.join([priv_dir(app), "repo", "migrations"])

  defp seeds_path(app), do: Path.join([priv_dir(app), "repo", "seeds.exs"])
end
