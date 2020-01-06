defmodule Wocky.Server.Metadata.ServerVersion do
  @moduledoc """
  Generate metadata containing server version"
  """

  @behaviour Wocky.Server.Metadata.DynamicMetadata

  def get do
    version =
      __MODULE__
      |> Application.get_application()
      |> Application.spec()
      |> Keyword.get(:vsn)
      |> to_string()

    {"server_version", version, "Current version of the Wocky server"}
  end
end
