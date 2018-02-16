defmodule WockyAPI.Schema do
  @moduledoc "GraphQL schema for the Wocky API"

  use Absinthe.Schema

  alias WockyAPI.UserResolver

  object :user do
    field :id, non_null(:string)
    field :handle, non_null(:string)
  end

  query do
    field :all_users, non_null(list_of(non_null(:user))) do
      resolve &UserResolver.all_users/3
    end
  end

  mutation do
    field :set_handle, :user do
      arg :id, non_null(:string)
      arg :handle, non_null(:string)

      resolve &UserResolver.set_handle/3
    end
  end
end
