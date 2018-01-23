defmodule Wocky.Repo do
  use Ecto.Repo, otp_app: :wocky

  alias Confex.Resolver
  alias Ecto.Repo.Supervisor
  alias Wocky.Repo.Instrumenter, as: RepoInstrumenter

  @doc """
  Dynamically loads the repository configuration from the environment variables.
  """
  def init(_, opts) do
    # The application isn't started when we are running migrations, so we have
    # to ensure that PrometheusEx is started and setup the Ecto instrumenter's
    # internal state.
    Application.ensure_all_started(:prometheus_ex)
    RepoInstrumenter.setup()

    url = System.get_env("DATABASE_URL")

    config =
      if url,
        do: Keyword.merge(opts, Supervisor.parse_url(url)),
        else: Resolver.resolve!(opts)

    unless config[:database] do
      raise "Set WOCKY_DB_NAME environment variable!"
    end

    unless config[:username] do
      raise "Set WOCKY_DB_USER environment variable!"
    end

    unless config[:password] do
      raise "Set WOCKY_DB_PASSWORD environment variable!"
    end

    unless config[:hostname] do
      raise "Set WOCKY_DB_HOST environment variable!"
    end

    unless config[:port] do
      raise "Set WOCKY_DB_PORT environment variable!"
    end

    {:ok, config}
  end
end
