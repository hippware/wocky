defmodule Wocky.Config.VaultAdapter do
  @moduledoc """
  Adapter to allow Confex to read secrets from Vault.
  """

  use ModuleConfig, otp_app: :wocky

  alias Vaultex.Client, as: Vaultex

  @behaviour Confex.Adapter

  @impl true
  def fetch_value(key) do
    base_path = get_config(:vault_prefix)

    case Vaultex.read(base_path <> key, :aws_iam, {nil, nil}) do
      {:ok, %{"value" => value}} -> {:ok, value}
      _ -> :error
    end
  end
end
