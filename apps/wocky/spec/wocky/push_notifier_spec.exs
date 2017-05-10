defmodule Wocky.PushNotifierSpec do
  use ESpec
  use Wocky.JID

  import Ecto.Query

  alias Faker.Code
  alias Wocky.Device
  alias Wocky.PushNotifier
  alias Wocky.PushNotifier.TestBackend
  alias Wocky.Repo
  alias Wocky.Repo.Factory

  @resource "testing"
  @platform "apple"
  @message  "Message content"

  before do
    TestBackend.reset

    user = Factory.insert(:user, %{server: shared.server})
    jid = JID.make(user.username, user.server, @resource)

    device_id = Code.isbn13
    result = PushNotifier.enable(jid, @platform, device_id)
    {:ok, user: user, jid: jid, device_id: device_id, result: result}
  end

  defp get_user_device(user, resource) do
    user
    |> Ecto.assoc(:devices)
    |> where([d], d.resource == ^resource)
    |> Repo.one
  end

  describe "enable/3" do
    context "on success" do
      it "should return :ok" do
        shared.result |> should(be_ok_result())
      end

      it "should register the device with the backend" do
        [{_, jid, platform, device_id}] = TestBackend.get_registrations
        jid |> should(eq JID.to_binary(shared.jid))
        platform |> should(eq @platform)
        device_id |> should(eq shared.device_id)
      end

      it "should insert the device_id and endpoint into the database" do
        row = get_user_device(shared.user, @resource)
        {:ok, endpoint} = shared.result

        row.device |> should(eq shared.device_id)
        row.endpoint |> should(eq endpoint)
      end
    end

    context "on failure" do
      before do
        Repo.delete_all(Device)
        TestBackend.reset

        result = PushNotifier.enable(shared.jid, @platform, "error")
        {:ok, result: result}
      end

      it "should return an error" do
        shared.result |> should(be_error_result())
      end

      it "should not register the device" do
        TestBackend.get_registrations |> should(eq [])
      end

      it "should not insert anything into the database" do
        get_user_device(shared.user, @resource) |> should(be_nil())
      end
    end
  end

  describe "disable/1" do
    before do
      result = PushNotifier.disable(shared.jid)
      {:ok, result: result}
    end

    it "should return :ok" do
      shared.result |> should(eq :ok)
    end

    it "should remove the device registration" do
      TestBackend.get_registrations |> should(eq [])
    end

    it "should remove the database records" do
      get_user_device(shared.user, @resource) |> should(be_nil())
    end
  end

  describe "delete/1" do
    before do
      other_jid = JID.replace_resource(shared.jid, "other")
      _ = PushNotifier.enable(other_jid, @platform, "987654321")
      result = PushNotifier.delete(shared.jid)
      {:ok, result: result}
    end

    it "should return :ok" do
      shared.result |> should(eq :ok)
    end

    it "should remove all device registrations" do
      TestBackend.get_registrations |> should(eq [])
    end

    it "should remove all database records" do
      get_user_device(shared.user, @resource) |> should(be_nil())
    end
  end

  describe "push/2" do
    context "on success" do
      before do
        result = PushNotifier.push(shared.jid, @message)
        {:ok, result: result}
      end

      it "should return :ok" do
        shared.result |> should(eq :ok)
      end

      it "should send a push notification" do
        [{_, message}] = TestBackend.get_notifications
        message |> should(eq @message)
      end
    end

    context "on failure" do
      before do
        result = PushNotifier.push(shared.jid, "error")
        {:ok, result: result}
      end

      it "should return :ok" do
        shared.result |> should(eq :ok)
      end
    end
  end

  describe "push_all/2" do
    before do
      other_jid = JID.replace_resource(shared.jid, "other")
      _ = PushNotifier.enable(other_jid, @platform, "987654321")
      result = PushNotifier.push_all(shared.jid, @message)
      {:ok, result: result}
    end

    it "should return :ok" do
      shared.result |> should(eq :ok)
    end

    it "should send a push notification to each endpoint" do
      TestBackend.get_notifications |> should(have_size 2)
    end
  end
end
