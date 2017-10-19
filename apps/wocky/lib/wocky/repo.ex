defmodule Wocky.Repo do
  use Ecto.Repo, otp_app: :wocky

  @doc """
  Dynamically loads the repository configuration from the environment variables.
  """
  def init(_, opts) do
    url = System.get_env("DATABASE_URL")
    config = if url,
      do: Keyword.merge(opts, Ecto.Repo.Supervisor.parse_url(url)),
      else: Confex.Resolver.resolve!(opts)

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
