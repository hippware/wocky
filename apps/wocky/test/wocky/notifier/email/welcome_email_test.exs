defmodule Wocky.Notifier.Email.WelcomeEmailTest do
  use Wocky.DataCase, async: false
  use Bamboo.Test

  alias Faker.Internet
  alias Faker.Name
  alias Wocky.Account
  alias Wocky.Account.User
  alias Wocky.Notifier.Email.WelcomeEmail
  alias Wocky.Repo
  alias Wocky.Repo.Factory

  test "send/1" do
    user = Factory.build(:user)

    assert WelcomeEmail.send(user) == :ok

    assert_email_delivered_with(to: [{user.name, user.email}])
  end

  describe "Email sending on user update" do
    setup do
      Application.put_env(:wocky, :send_welcome_email, true)

      :meck.new(WelcomeEmail)
      :meck.expect(WelcomeEmail, :send, fn %User{} -> :ok end)

      on_exit(fn ->
        :meck.unload()
        Application.put_env(:wocky, :send_welcome_email, false)
      end)

      :ok
    end

    test "should be sent on user update" do
      user = Factory.insert(:user)

      fields = %{
        name: Name.name()
      }

      Account.update(user.id, fields)

      assert :meck.validate(WelcomeEmail)
      assert :meck.called(WelcomeEmail, :send, :_)

      new_user = Repo.get(User, user.id)
      assert new_user.welcome_sent
    end

    test "should not be resent to an already welcomed user" do
      user = Factory.insert(:user, %{welcome_sent: true})

      fields = %{
        email: Internet.email()
      }

      Account.update(user.id, fields)

      refute :meck.called(WelcomeEmail, :send, :_)
    end

    test "should not be sent to an unwelcomed user that has no email" do
      user = Factory.insert(:user, %{email: nil})

      fields = %{
        name: Name.name()
      }

      Account.update(user.id, fields)

      refute :meck.called(WelcomeEmail, :send, :_)
    end
  end
end
