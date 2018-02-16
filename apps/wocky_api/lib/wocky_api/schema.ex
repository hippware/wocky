defmodule WockyAPI.Schema do
  @moduledoc "GraphQL schema for the Wocky API"

  use Absinthe.Schema

  alias WockyAPI.UserResolver

  object :user do
    field :profile, non_null(:profile)
    field :home_stream, non_null(list_of(non_null(:home_stream_item)))
    field :contacts, non_null(list_of(non_null(:contact))) do
      arg :relationship, :relationship
    end
    field :conversations, non_null(list_of(non_null(:conversation)))
    field :bots, non_null(list_of(non_null(:bot)))
    field :shares, non_null(list_of(non_null(:bot)))
    field :subscriptions, non_null(list_of(non_null(:bot)))
    field :visible_users, non_null(list_of(non_null(:profile)))
  end

  object :profile do
    field :id, non_null(:string)
    field :username, non_null(:string)
    field :handle, non_null(:string)
    field :avatar, non_null(:string)
    field :first_name, non_null(:string)
    field :last_name, non_null(:string)
    field :tagline, :string
    field :roles, non_null(list_of(non_null(:string)))
  end

  object :home_stream_item do
    field :id, non_null(:string)
  end

  enum :relationship do
    value :following
    value :follower
    value :friend
  end

  object :contact do
    field :relationship, non_null(:relationship)
    field :profile, non_null(:profile)
  end

  object :conversation do
    field :id, non_null(:string)
  end

  object :bot do
    field :id, non_null(:string)
  end

  query do
    field :current_user, non_null(:user) do
      resolve &UserResolver.current_user/3
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
