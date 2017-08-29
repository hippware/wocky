
unittest:
        mix do lint, ecto.wait, ecto.reset, test, espec

inttest:
        mix do ecto.wait, ecto.reset, epmd
        mix ct

release:
        MIX_ENV=prod mix release --warnings-as-errors
        cp _build/prod/rel/wocky/releases/`elixir ./version.exs`/wocky.tar.gz /artifacts

# Builds a release image locally and pushes to DockerHub
local:
        rm -f ${PWD}/tmp/artifacts/wocky.tar.gz
        docker build . -t hippware/wocky:build -f Dockerfile.build
        docker run -it --rm -v ${PWD}/tmp/artifacts:/artifacts hippware/wocky:build make release
        docker build . -t hippware/wocky:latest -f Dockerfile.release
        docker push hippware/wocky:latest
