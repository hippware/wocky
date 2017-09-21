defmodule Wocky.EmailSpec do
  use ExUnit.Case
  use Bamboo.Test

  alias Wocky.Email
  alias Wocky.Repo.Factory

  setup do
    {:ok, user: Factory.build(:user)}
  end

  test "send_welcome_email/1", context do
    assert Email.send_welcome_email(context.user) == :ok
    assert_delivered_with(
      to: [{context.user.first_name <> " " <> context.user.last_name,
            context.user.email}])
  end
end
