defmodule Wocky.Config.VaultAdapter do
  @moduledoc """
  Adapter to allow Confex to read secrets from Vault.
  """

  @behaviour Confex.Adapter

  use ModuleConfig, otp_app: :wocky

  alias Vaultex.Client, as: Vaultex

  require Logger

  @impl true
  def fetch_value(key) do
    path = get_config(:vault_prefix) <> key

    _ = Logger.info("Fetching #{key} from #{path} in Vault")

    case Vaultex.read(path, :aws_iam, {nil, nil}) do
      {:ok, %{"value" => value}} ->
        {:ok, value}

      error ->
        _ = Logger.error("Error fetching #{key}: #{inspect(error)}")
        :error
    end
  end
end
