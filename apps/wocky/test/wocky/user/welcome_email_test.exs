defmodule Wocky.User.WelcomeEmailTest do
  use Wocky.DataCase, async: false
  use Bamboo.Test

  alias Faker.{Internet, Name}
  alias Wocky.{Repo, User}
  alias Wocky.Repo.Factory
  alias Wocky.User.WelcomeEmail

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

      User.update(user.id, fields)

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

      User.update(user.id, fields)

      refute :meck.called(WelcomeEmail, :send, :_)
    end

    test "should not be sent to an unwelcomed user that has no email" do
      user = Factory.insert(:user, %{email: nil})

      fields = %{
        name: Name.name()
      }

      User.update(user.id, fields)

      refute :meck.called(WelcomeEmail, :send, :_)
    end
  end
end
