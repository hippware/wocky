defmodule WockyAPI.Callbacks.Notification do
  @moduledoc """
  Callbacks for notification changes
  """

  use DawdleDB.Handler, type: Wocky.Notifier.InBand.Notification

  alias Absinthe.Subscription
  alias Wocky.Repo.Hydrator
  alias WockyAPI.Endpoint
  alias WockyAPI.Resolvers.Notification

  def handle_insert(new) do
    Hydrator.with_assocs(new, [:other_user], fn rec ->
      rec
      |> Notification.to_graphql()
      |> publish(rec.user_id)
    end)
  end

  def handle_delete(old) do
    %{id: old.id}
    |> publish(old.user_id)
  end

  defp publish(data, user_id) do
    topic = Notification.notification_subscription_topic(user_id)

    Subscription.publish(Endpoint, data, [{:notifications, topic}])
  end
end
