defmodule Wocky.Config do
  @moduledoc "Simplifies and organizes module configuration"

  @doc false
  defmacro __using__(_opts) do
    quote location: :keep do
      @doc """
      Returns module configuration.
      """
      @spec config(Keyword.t()) :: map()
      def config(opts \\ []) do
        Confex.fetch_env!(:wocky, __MODULE__)
        |> Keyword.merge(opts)
        |> Enum.into(%{})
      end

      @doc """
      Returns a specific module configuration value.
      """
      @spec get_config(atom()) :: any()
      def get_config(key) do
        Map.get(config(), key, nil)
      end
    end
  end
end
