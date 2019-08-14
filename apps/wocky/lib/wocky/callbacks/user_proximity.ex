defmodule Wocky.Callbacks.UserProximity do
  @moduledoc "DB callback handler for proximity subscriptions"

  use DawdleDB.Handler, type: Wocky.Location.UserProximity.Subscription

  alias Wocky.Location

  def handle_insert(new) do
    call_updates(&Location.add_proximity_subscription/2, new)
  end

  def handle_update(new, _old) do
    call_updates(&Location.add_proximity_subscription/2, new)
  end

  def handle_delete(old) do
    call_updates(&Location.remove_proximity_subscription/2, old)
  end

  defp call_updates(fun, sub) do
    fun.(sub.user_id, sub)
    fun.(sub.target_id, sub)
  end
end
