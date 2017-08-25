
unittest:
        mix do lint, ecto.wait, ecto.reset, test, espec

inttest:
        mix do ecto.wait, ecto.reset, epmd
        mix ct

release:
        MIX_ENV=prod mix release --warnings-as-errors
        cp _build/prod/rel/wocky/releases/`elixir ./version.exs`/wocky.tar.gz /artifacts
