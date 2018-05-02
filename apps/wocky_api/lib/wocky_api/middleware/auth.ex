defmodule WockyAPI.Middleware.Auth do
  @moduledoc """
  Helper functions for implementing scope-based authorization using
  `WockyAPI.Schema.Notation`'s `scope` macro.
  """
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
    %{__private__: field_priv},
    %{__private__: obj_priv}
  ) do
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

  defp scope_middleware(middleware, :public) do
    middleware
  end

  defp scope_middleware(middleware, :authenticated) do
    [WockyAPI.Middleware.AuthUser | middleware]
  end

  defp scope_middleware(middleware, :private) do
    [WockyAPI.Middleware.AuthSelf | middleware]
  end
end
