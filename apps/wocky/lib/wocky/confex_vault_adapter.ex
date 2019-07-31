defmodule Wocky.ConfexVaultAdapter do
  @moduledoc """
  Adapter to allow Confex to read secrets from Vault.
  """

  use ModuleConfig, otp_app: :wocky

  import Cachex.Spec

  alias Vaultex.Client, as: Vaultex

  @behaviour Confex.Adapter

  def child_spec(_) do
    %{
      id: VaultCache,
      start:
        {Cachex, :start_link,
         [
           :vault_cache,
           [expiration: expiration(default: :timer.hours(1), interval: nil)]
         ]}
    }
  end

  @impl true
  def fetch_value(key) do
    case Cachex.fetch(:vault_cache, key, &get_from_vault/1) do
      {result, value} when result in [:ok, :commit, :ignore] -> value
      _ -> :error
    end
  end

  defp get_from_vault(key) do
    base_path = get_config(:vault_prefix)

    case Vaultex.read(base_path <> key, :aws_iam, {nil, nil}) do
      {:ok, %{"value" => value}} -> {:commit, {:ok, value}}
      _ -> {:commit, :error}
    end
  end
end
