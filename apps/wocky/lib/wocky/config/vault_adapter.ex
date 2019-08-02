defmodule Wocky.Config.VaultAdapter do
  @moduledoc """
  Adapter to allow Confex to read secrets from Vault.
  """

  use ModuleConfig, otp_app: :wocky

  require Logger

  alias Vaultex.Client, as: Vaultex

  @behaviour Confex.Adapter

  @impl true
  def fetch_value(key) do
    path = get_config(:vault_prefix) <> key

    Logger.info("Fetching #{key} from #{path} in Vault")

    case Vaultex.read(path, :aws_iam, {nil, nil}) do
      {:ok, %{"value" => value}} ->
        {:ok, value}
      error ->
        Logger.error("Error fetching #{key}: #{inspect error}")
        :error
    end
  end
end
