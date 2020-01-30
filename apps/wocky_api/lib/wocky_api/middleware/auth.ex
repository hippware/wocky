defmodule WockyAPI.Middleware.Auth do
  @moduledoc """
  Helper functions for implementing scope-based authorization using
  `WockyAPI.Schema.Notation`'s `scope` macro.
  """

  @spec middleware(list(), map(), map()) :: list()
  def middleware(middleware, %{__private__: field}, %{__private__: obj}) do
    scope =
      case obj[:meta][:scope] do
        nil -> field[:meta][:scope]
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
