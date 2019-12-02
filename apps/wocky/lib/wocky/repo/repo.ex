defmodule Wocky.Repo do
  use Ecto.Repo,
    otp_app: :wocky,
    adapter: Ecto.Adapters.Postgres

  use Paginator

  alias Ecto.Association.NotLoaded
  alias Confex.Resolver
  alias Wocky.Repo.Instrumenter, as: RepoInstrumenter

  @type error :: {:error, Ecto.Changeset.t()}
  @type success(schema_type) :: {:ok, schema_type}
  @type result(schema_type) :: success(schema_type) | error()
  @type not_loaded :: %NotLoaded{}

  @doc """
  Dynamically loads the repository configuration from the environment variables.
  """
  @impl true
  def init(_, opts) do
    # The application isn't started when we are running migrations, so we have
    # to ensure that PrometheusEx is started and setup the Ecto instrumenter's
    # internal state.
    {:ok, _} = Application.ensure_all_started(:prometheus_ex)

    RepoInstrumenter.setup()

    config = Resolver.resolve!(opts)

    {:ok, config}
  end
end
