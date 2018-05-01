defmodule WockyAPI.Schema.Notation do
  @moduledoc "Helper macros for common wocky schema definitions"

  defmacro __using__(_) do
    quote do
      use Absinthe.Schema.Notation
      use Absinthe.Relay.Schema.Notation, :modern

      import unquote(__MODULE__)
    end
  end

  defmacro changeset_mutation_middleware do
    quote do
      middleware &WockyAPI.Resolvers.Utils.fix_changeset/2
      middleware &build_payload/2
    end
  end

  defmacro total_count_field do
    quote do
      field :total_count, non_null(:integer) do
        resolve &WockyAPI.Resolvers.Utils.get_count/3
      end
    end
  end
end
