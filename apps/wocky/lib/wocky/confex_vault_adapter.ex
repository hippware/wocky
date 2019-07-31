defmodule Wocky.ConfexVaultAdapter do
  @moduledoc """
  Adapter to allow Confex to read secrets from Vault.
  In environments where Vault is not available (`use_vault` is set to `false`)
  the value will be read from `WOCKY_VAULT_<key>`.
  """

  use ModuleConfig, otp_app: :wocky

  import Cachex.Spec

  alias Vaultex.Client, as: Vaultex

  @behaviour Confex.Adapter

  def start_link() do
    Cachex.start_link(:vault_cache,
      expiration: expiration(default: :timer.hours(1), interval: nil)
    )
  end

  @impl true
  def fetch_value(key) do
    if get_config(:use_vault, true) do
      get_vault_value(key)
    else
      get_env_value(key)
    end
  end

  defp get_vault_value(key),
    do: Cachex.fetch(:vault_cache, key, &get_from_vault/1)

  defp get_from_vault(key) do
    base_path = get_config(:vault_prefix)
    case Vaultex.read(base_path <> key, :aws_iam, {nil, nil}) do
      {:ok, %{"value" => value}} -> {:commit, {:ok, value}}
      _ -> {:commit, :error}
    end
  end

  defp get_env_value(key) do
    case System.get_env("WOCKY_VAULT_" <> key) do
      nil -> :error
      value -> {:ok, value}
    end
  end
end
