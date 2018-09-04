defmodule WockyAPI.Schema.Notation do
  @moduledoc "Helper macros for common wocky schema definitions"

  defmacro __using__(_) do
    quote do
      use Absinthe.Schema.Notation
      use Absinthe.Relay.Schema.Notation, :modern

      import unquote(__MODULE__), only: :macros
    end
  end

  defmacro changeset_mutation_middleware do
    quote do
      middleware &WockyAPI.Resolvers.Utils.fix_changeset/2
      middleware &build_payload/2
    end
  end

  defmacro changeset_list_mutation_middleware do
    quote do
      middleware &WockyAPI.Resolvers.Utils.fix_changeset_list/2
    end
  end

  defmacro total_count_field do
    quote do
      field :total_count, non_null(:integer) do
        resolve &WockyAPI.Resolvers.Utils.get_count/3
      end
    end
  end

  defmacro scope(scope) do
    quote do
      meta :scope, unquote(scope)
    end
  end

  defmacro connection_complexity do
    quote do
      complexity &unquote(__MODULE__).connection_complexity/2
    end
  end

  defmacro user_subscription_config(topic_fun) do
    quote do
      config fn
        _, %{context: %{current_user: user}} ->
          {:ok, topic: unquote(topic_fun).(user.id)}

        _, _ ->
          {:error, "This operation requires an authenticated user"}
      end
    end
  end

  def connection_complexity(params, children) do
    count = params[:first] || params[:last] || 1
    count * children
  end
end
