defmodule Wocky.User.WelcomeEmailTest do
  use Wocky.DataCase, async: false

  alias Faker.{Internet, Name}
  alias Wocky.{Email, Repo, User}
  alias Wocky.Repo.Factory

  setup do
    Application.put_env(:wocky, :send_welcome_email, true)

    :meck.new(Email)
    :meck.expect(Email, :send_welcome_email, fn %User{} -> :ok end)

    on_exit(fn ->
      :meck.unload()
      Application.put_env(:wocky, :send_welcome_email, false)
    end)

    :ok
  end

  test "should be sent on user update" do
    user = Factory.insert(:user)

    fields = %{
      first_name: Name.first_name()
    }

    User.update(user.id, fields)

    assert :meck.validate(Email)
    assert :meck.called(Email, :send_welcome_email, :_)

    new_user = Repo.get(User, user.id)
    assert new_user.welcome_sent
  end

  test "should not be resent to an already welcomed user" do
    user = Factory.insert(:user, %{welcome_sent: true})

    fields = %{
      email: Internet.email()
    }

    User.update(user.id, fields)

    refute :meck.called(Email, :send_welcome_email, :_)
  end

  test "should not be sent to an unwelcomed user that has no email" do
    user = Factory.insert(:user, %{email: nil})

    fields = %{
      first_name: Name.first_name()
    }

    User.update(user.id, fields)

    refute :meck.called(Email, :send_welcome_email, :_)
  end
end
