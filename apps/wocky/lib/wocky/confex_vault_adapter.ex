defmodule Wocky.ConfexVaultAdapter do
  @moduledoc """
  Adapter to allow Confex to read secrets from Vault.
  In environments where Vault is not available (`use_vault` is set to `false`)
  the value will be read from `WOCKY_VAULT_<key>`.
  """

  use ModuleConfig, otp_app: :wocky

  alias Vaultex.Client, as: Vaultex

  @behaviour Confex.Adapter

  @impl true
  def fetch_value(key) do
    if get_config(:use_vault, true) do
      get_vault_value(key)
    else
      get_env_value(key)
    end
  end

  defp get_vault_value(key) do
    base_path = "secret/wocky-" <> Confex.get_env(:wocky, :wocky_env) <> "/"
    case Vaultex.read(base_path <> key, :aws_iam, {nil, nil}) do
      {:ok, %{"value" => value}} -> {:ok, value}
      _ -> :error
    end
  end

  def get_env_value(key) do
    case System.get_env("WOCKY_VAULT_" <> key) do
      nil -> :error
      value -> {:ok, value}
    end
  end
end
