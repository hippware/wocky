defmodule Wocky.Callbacks.UserProximity do
  @moduledoc "DB callback handler for proximity subscriptions"

  use DawdleDB.Handler, type: Wocky.Location.UserProximity.Subscription

  alias Wocky.Location

  def handle_insert(new) do
    call_updates(new)
  end

  def handle_update(new, _old) do
    call_updates(new)
  end

  def handle_delete(old) do
    call_updates(old)
  end

  defp call_updates(sub) do
    Location.refresh_proximity_subscriptions(sub.user_id)
    Location.refresh_proximity_subscriptions(sub.target_id)
  end
end
