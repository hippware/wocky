defmodule Wocky.UserHelper do
  @moduledoc "Helper functions for user management in tests"

  alias Wocky.Account.User
  alias Wocky.Contacts
  alias Wocky.NotificationHelper

  @doc "Befriend users while clearing generated notifications"
  @spec befriend(User.t(), User.t()) :: :ok
  def befriend(user1, user2) do
    _ = Contacts.befriend(user1, user2)
    NotificationHelper.clear_expected_notifications(user1, 1)
    NotificationHelper.clear_expected_notifications(user2, 1)
  end
end
