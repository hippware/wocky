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

  def init(_) do
    _ = Confex.resolve_env!(:ex_aws)
    _ = Confex.resolve_env!(:vaultex)

    {:ok, _} = Application.ensure_all_started(:ex_aws)
    {:ok, _} = Application.ensure_all_started(:vaultex)

    Application.loaded_applications()
    |> Enum.each(fn {app, _desc, _ver} ->
      Confex.resolve_env!(app, persistent: true)
    end)
  end
end
