defmodule Wocky.EmailSpec do
  use ExUnit.Case
  use Bamboo.Test

  alias Wocky.Email
  alias Wocky.Repo.Factory
  alias Wocky.User

  setup do
    {:ok, user: Factory.build(:user)}
  end

  test "send_welcome_email/1", context do
    assert Email.send_welcome_email(context.user) == :ok

    assert_email_delivered_with(
      to: [{User.full_name(context.user), context.user.email}]
    )
  end
end
