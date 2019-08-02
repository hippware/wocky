defmodule Wocky.Config.ConfexProvider do
  @moduledoc """
  Distillery config provider for pre-populating config data using Confex.

  We need to run this as a provider rather than at app runtime because one of
  our Confex adapters requires Vaultex to be running. Unfortunately there's no
  way to ensure Vaultex is running before the other apps that require it to set
  up their config. Fortunately Distillery providers give us a way around this by
  running before the full dependency app set is started and hard wiring some
  runtime config.
  """

  use Distillery.Releases.Config.Provider

  alias Config.Reader

  def init([config_file]) do
    _ = Confex.resolve_env!(:ex_aws)
    _ = Confex.resolve_env!(:vaultex)

    {:ok, _} = Application.ensure_all_started(:ex_aws)
    {:ok, _} = Application.ensure_all_started(:vaultex)

    {:ok, file} = Provider.expand_path(config_file)
    config = Reader.read!(file)

    Enum.each(config, fn {app, config} -> put_config(app, config) end)

    config
    |> Keyword.keys()
    |> Enum.each(&Confex.resolve_env!(&1, persistent: true))
  end

  defp put_config(app, config) do
    Enum.each(config, fn {key, value} ->
      Application.put_env(app, key, value, persistent: true)
    end)
  end
end
