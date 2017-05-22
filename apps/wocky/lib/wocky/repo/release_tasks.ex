defmodule Wocky.Repo.ReleaseTasks do
  @moduledoc false

  alias Ecto.Migrator

  @app :wocky
  @repo Wocky.Repo
  @start_apps [
    :postgrex,
    :ecto
  ]

  def seed do
    IO.puts "=> Loading #{@app}..."
    # Load the code for app, but don't start it
    :ok = Application.load(@app)

    IO.puts "=> Starting dependencies..."
    # Start apps necessary for executing migrations
    Enum.each(@start_apps, &Application.ensure_all_started/1)

    # Start the Repo(s) for myapp
    IO.puts "=> Starting repo..."
    @repo.start_link(pool_size: 1)

    # Run migrations
    IO.puts "=> Running migrations for #{@app}..."
    Migrator.run(@repo, migrations_path(@app), :up, all: true)

    # Run the seed script if it exists
    seed_script = seeds_path(@app)
    if File.exists?(seed_script) do
      IO.puts "=> Running seed script..."
      Code.eval_file(seed_script)
    end

    # Signal shutdown
    IO.puts "=> Success!"
    :init.stop()
  end

  defp priv_dir(app), do: "#{:code.priv_dir(app)}"

  defp migrations_path(app),
    do: Path.join([priv_dir(app), "repo", "migrations"])

  defp seeds_path(app),
    do: Path.join([priv_dir(app), "repo", "seeds.exs"])
end
