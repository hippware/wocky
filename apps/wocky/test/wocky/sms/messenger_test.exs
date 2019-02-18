defmodule Wocky.SMS.MessengerTest do
  use Wocky.DataCase, async: true

  alias Wocky.Repo.Factory
  alias Wocky.SMS.Messenger
  alias Wocky.User

  describe "send/2" do
    test "should send successfully" do
      user = Factory.insert(:user)
      assert :ok == Messenger.send("+15551234567", "testing", user)

      assert Repo.get(User, user.id).smss_sent == 1
    end

    test "should refuse to send if the user has reached their SMS limit" do
      user =
        Factory.insert(:user,
          smss_sent: Confex.get_env(:wocky, :max_sms_per_user)
        )

      assert {:error, :user_sms_limit_reached} ==
               Messenger.send("+15551234567", "testing", user)
    end

    test """
    should not refuse to send if the user has reached their SMS limit on
    numbers configured as unlimited
    """ do
      user =
        Factory.insert(:user,
          smss_sent: Confex.get_env(:wocky, :max_sms_per_user)
        )

      Application.put_env(:wocky, :unlimited_sms_numbers, [user.phone_number])

      assert :ok == Messenger.send("+15551234567", "testing", user)
    end
  end
end
