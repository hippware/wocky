defmodule WockyAPI.Endpoint do
  use Phoenix.Endpoint, otp_app: :wocky_api

  use Absinthe.Phoenix.Endpoint

  # Websocket to enable GraphQL subscription magic
  socket "/graphql", WockyAPI.Channels.UserSocket,
    websocket: [connect_info: [:peer_data]]

  plug CORSPlug

  # Serve at "/" the static files from "priv/static" directory.
  #
  # You should set gzip to true if you are running phoenix.digest
  # when deploying your static files in production.
  # Not serving static content right now
  # plug Plug.Static,
  #  at: "/", from: :wocky_api, gzip: false,
  #  only: ~w(css fonts images js favicon.ico robots.txt)

  # Code reloading can be explicitly enabled under the
  # :code_reloader configuration of your endpoint.
  if code_reloading? do
    plug Phoenix.CodeReloader
  end

  plug Plug.RequestId
  plug Plug.Logger, log: :debug

  plug Plug.Parsers,
    parsers: [:urlencoded, :multipart, :json],
    pass: ["*/*"],
    json_decoder: Poison

  plug Plug.MethodOverride
  plug Plug.Head

  # The session will be stored in the cookie and signed,
  # this means its contents can be read but not tampered with.
  # Set :encryption_salt if you would also like to encrypt it.
  # No session tracking right now
  # plug Plug.Session,
  #  store: :cookie,
  #  key: "_wocky_api_key",
  #  signing_salt: "JqDu/CcJ"

  # measures pipeline exec times
  plug WockyAPI.Metrics.PipelineInstrumenter
  plug WockyAPI.Router

  # Callback invoked for dynamically configuring the endpoint.
  #
  # It receives the endpoint configuration and checks if
  # configuration should be loaded from the system environment.
  @impl true
  def init(_key, config) do
    if config[:load_from_system_env] do
      port =
        System.get_env("PORT") ||
          raise "expected the PORT environment variable to be set"

      {:ok, Keyword.put(config, :http, [:inet6, port: port])}
    else
      {:ok, config}
    end
  end
end
