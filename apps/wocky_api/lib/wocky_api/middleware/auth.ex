defmodule WockyAPI.Middleware.Auth do
  @moduledoc """
  Helper functions for implementing scope-based authentication using
  `WockyAPI.Schema.Notation`'s `scope` macro.
  """
  defmacro __using__(_) do
    quote do
      @schema_fields [
        :__schema,
        :__type,
        :__enumvalue,
        :__directive,
        :__inputvalue,
        :__field
      ]

      def middleware(middleware, _field, %{identifier: object})
      when object in @schema_fields do
        scope_middleware(middleware, :public)
      end

      def middleware(
        middleware,
        %{__private__: field_priv} = f,
        %{__private__: obj_priv} = object) do
          scope =
            case obj_priv[:meta][:scope] do
              nil -> field_priv[:meta][:scope]
              obj_scope -> obj_scope
            end
          scope_middleware(middleware, scope || :authenticated)
        end

      def middleware(middleware, _field, _object) do
        scope_middleware(middleware, :authenticated)
      end

      def scope_middleware(middleware, :public) do
        middleware
      end

      def scope_middleware(middleware, :authenticated) do
        [WockyAPI.Middleware.AuthUser | middleware]
      end

      def scope_middleware(middleware, :private) do
        [WockyAPI.Middleware.AuthSelf | middleware]
      end
    end
  end
end
