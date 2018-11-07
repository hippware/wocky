defmodule WockyAPI.Presence do
  @moduledoc """
  This module provides the interface for user presence management in Wocky
  """

  alias Absinthe.Subscription
  alias Wocky.User
  alias WockyAPI.Endpoint
  alias WockyAPI.Presence.{Manager, Store}
  alias WockyAPI.Resolvers.User, as: UserResolver

  @type status() :: :offline | :online

  @doc """
  Mark a user online and return a list of their currently-online followees
  """
  @spec connect(User.t()) :: [User.t()]
  def connect(user) do
    {:ok, manager} = Manager.acquire(user)

    manager
    |> Manager.online_contacts()
    |> Enum.map(&User.get_user/1)
  end

  @doc "Get the online status for a given user"
  @spec user_status(User.t()) :: status()
  def user_status(user) do
    case Store.get(user.id) do
      nil -> :offline
      _ -> :online
    end
  end

  @doc """
  Publish given online status of a user (`contact`) to another user
  (`recipient`)
  """
  @spec publish(User.id(), User.t()) :: :ok
  def publish(recipient_id, contact) do
    Subscription.publish(
      Endpoint,
      contact,
      [{:followees, UserResolver.followees_subscription_topic(recipient_id)}]
    )

    :ok
  end
end
