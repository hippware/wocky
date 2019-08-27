defmodule Wocky.Location.UserProximity do
  @moduledoc """
  Module for handling user proximity subscription operations
  """

  import Ecto.Query

  alias Wocky.Account.User
  alias Wocky.Events.UserProximity, as: ProxEvent
  alias Wocky.Location
  alias Wocky.Location.Handler
  alias Wocky.Location.UserLocation
  alias Wocky.Location.UserLocation.Current
  alias Wocky.Location.UserProximity.Subscription
  alias Wocky.Notifier
  alias Wocky.Repo

  @spec get_subscriptions(User.t()) :: [Subscription.t()]
  def get_subscriptions(user) do
    Subscription
    |> where(user_id: ^user.id)
    |> preload([:target])
    |> Repo.all()
  end

  @spec get_subscribers(User.t()) :: [Subscription.t()]
  def get_subscribers(user) do
    Subscription
    |> where(target_id: ^user.id)
    |> preload([:user])
    |> Repo.all()
  end

  @spec notify_subscribers(User.t(), UserLocation.t(), [Subscription.t()]) ::
          :ok
  def notify_subscribers(user, location, subscriptions) do
    subscriptions
    |> Enum.each(&Handler.set_proximity_location(&1.user, user, location))
  end

  @spec check_targets(User.t(), UserLocation.t(), [Subscription.t()]) :: [
          Subscription.t()
        ]
  def check_targets(user, location, subscriptions) do
    subscriptions
    |> Enum.map(&check_target(user, location, &1))
  end

  defp check_target(user, user_location, subscription) do
    case should_check?(subscription) do
      false ->
        subscription

      true ->
        case Current.get(subscription.target) do
          nil ->
            subscription

          location ->
            process_location(user, user_location, subscription, location)
        end
    end
  end

  def check_for_notify(user, notified_location, subscription) do
    case should_check?(subscription) do
      false ->
        subscription

      true ->
        case Current.get(user) do
          nil ->
            subscription

          user_location ->
            process_location(
              user,
              user_location,
              subscription,
              notified_location
            )
        end
    end
  end

  defp process_location(user, user_location, subscription, location) do
    if Geocalc.within?(
         subscription.range,
         Location.to_point(user_location),
         Location.to_point(location)
       ) do
      notify(user, subscription)
    else
      subscription
    end
  end

  defp should_check?(%Subscription{last_notification: nil}), do: true

  defp should_check?(%Subscription{last_notification: last, cooldown: cooldown}) do
    result =
      last
      |> DateTime.add(cooldown, :millisecond)
      |> DateTime.compare(DateTime.utc_now())

    result == :lt
  end

  defp notify(user, subscription) do
    new_time = DateTime.utc_now()

    Subscription
    |> where(user_id: ^subscription.user_id)
    |> where(target_id: ^subscription.target_id)
    |> Repo.update_all(set: [last_notification: new_time])

    %ProxEvent{
      to: user,
      from: subscription.target
    }
    |> Notifier.notify()

    %{subscription | last_notification: new_time}
  end
end
